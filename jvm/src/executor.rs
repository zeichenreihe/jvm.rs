use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use std::mem::{size_of, size_of_val};
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use crate::classfile::{ClassFile, CpInfo, JUtf8, Parse};
use crate::executor::Error::WrongConstantPoolTag;
use crate::opcodes::Opcode;

type JBoolean = bool;//todo!();
type JByte = i8;
type JShort = i16;
type JChar = char;//todo!();
type JInt = i32;
type JLong = i64;
type JFloat = f32;
type JDouble = f64;
type JReference = u64; //todo!();

#[derive(Debug)]
enum Error {
	OverflowErr,
	Other(super::classfile::Error),
	WrongConstantPoolTag,
	ClassAlreadyLoaded(JUtf8),
	NoClassDefFound(JUtf8),
}

impl From<super::classfile::Error> for Error {
	fn from(value: crate::classfile::Error) -> Self {
		Self::Other(value)
	}
}

/// Represents the memory that contains the fields of a class
struct ClassInstance<const SIZE: usize> {
	// in memory:
	// <parent class instance><first field><second field><...><last field>
	data: Box<[u8; SIZE]>
}

impl<const SIZE: usize> ClassInstance<SIZE> {
	/// Returns the whole size of the class instance
	pub fn get_size(&self) -> usize {
		SIZE
	}

	pub fn get_int(&self, offset: usize) -> Result<JInt, errors::OutOfBoundsError> {
		let slice = self.data
			.get(offset..).ok_or(errors::OutOfBoundsError)?
			.get(..4).ok_or(errors::OutOfBoundsError)?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		Ok(JInt::from_ne_bytes(slice))
	}
	pub fn put_int(&mut self, offset: usize, int: JInt) -> Result<(), errors::OutOfBoundsError> {
		let slice = self.data
			.get_mut(offset..).ok_or(errors::OutOfBoundsError)?
			.get_mut(..size_of_val(&int)).ok_or(errors::OutOfBoundsError)?;
		slice.copy_from_slice(&int.to_ne_bytes());
		Ok(())
	}

	pub fn get_reference(&self, offset: usize) -> Result<JReference, ()> {
		todo!()
	}
	pub fn put_reference(&self, offset: usize, reference: JReference) -> Result<(), ()> {
		todo!()
	}

	// TODO: more!
}

/// Stores a [ClassInstance] and a count of references to it
struct ClassInstanceRefCounted<const SIZE: usize> {
	class_instance: Arc<ClassInstance<SIZE>>,
}

#[derive(Debug)]
struct Vm {
	classes: HashMap<JUtf8, ClassFile>,
	stack: VmStack,
}

impl Vm {
	fn new() -> Vm {
		Vm { classes: HashMap::new(), stack: VmStack { frames: Vec::new() }}
	}

	fn try_add_class<R: Read>(&mut self, mut reader: R) -> Result<(), Error> {
		let classfile = ClassFile::parse(&mut reader, None)?;
		let name = classfile.name()?.to_owned();

		match self.classes.insert(name.clone(), classfile) {
			Some(class) => Err(Error::ClassAlreadyLoaded(class.name()?.to_owned())),
			None => Ok(()),
		}
	}

	fn get_class(&self, name: &JUtf8) -> Result<&ClassFile, Error> {
		self.classes.get(name).ok_or_else(|| Error::NoClassDefFound(name.to_owned()))
	}

	fn start_at_current_frame(&mut self) -> Result<(), Error> {
		self.stack.frames.last_mut().ok_or(Error::OverflowErr)?.run_isn()
	}
}

#[derive(Debug)]
struct VmStack {
	frames: Vec<VmStackFrame>,
}

#[derive(Debug)]
struct VmStackFrame {
	local_variables: Vec<u32>,
	operand_stack: Vec<u32>,
	stack_pointer: usize,

	program_counter: usize,
	code: Vec<u8>,
	class: ClassFile,
}

impl Display for VmStackFrame {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "VmStackFrame: lvs: {:?}; stack: {:?}; sp: {:?}, pc: {:?}", self.local_variables, self.operand_stack, self.stack_pointer, self.program_counter)
	}
}

impl VmStackFrame {
	fn push_stack(&mut self, value: u32) -> Result<(), Error> {
		*self.operand_stack
			.get_mut(self.stack_pointer)
			.ok_or(Error::OverflowErr)? = value;
		self.stack_pointer += 1;
		Ok(())
	}

	fn pop_stack(&mut self) -> Result<u32, Error> {
		self.stack_pointer -= 1;
		self.operand_stack.get(self.stack_pointer)
			.ok_or(Error::OverflowErr)
			.map(|i| *i)
	}

	fn read_isn(&mut self) -> Result<u8, Error> {
		let value = self.code.get(self.program_counter)
			.ok_or(Error::OverflowErr)
			.map(|i| *i);
		self.program_counter += 1;
		value
	}

	fn read_constant_pool_two_indexes(&mut self) -> Result<usize, Error> {
		let index_byte1 = self.read_isn()? as usize;
		let index_byte2 = self.read_isn()? as usize;
		Ok(index_byte1 << 8 | index_byte2)
	}

	fn run_isn(&mut self) -> Result<(), Error> {
		loop {
			let opcode = Opcode::try_from(self.read_isn()?).expect("for now good enough");

			println!("opcode: {opcode:?}");

			// TODO: make this actually allocate memory
			match opcode {
				Opcode::IConst0 => self.push_stack(0)?,
				Opcode::IConst1 => self.push_stack(1)?,
				Opcode::IConst2 => self.push_stack(2)?,
				Opcode::IConst3 => self.push_stack(3)?,
				Opcode::IAdd => {
					let a = self.pop_stack()?;
					let b = self.pop_stack()?;
					self.push_stack(a + b)?
				},
				Opcode::GetStatic => {
					let index = self.read_constant_pool_two_indexes()?;
					let (class, name, desc) = if let CpInfo::FieldRef(field) = self.class.get_constant_pool(index)? {
						field.class_name_descriptor(&self.class)?
					} else {
						Err(WrongConstantPoolTag)?
					};

					println!("{:?}, {:?}, {:?}", String::from_utf8(class.to_vec()), String::from_utf8(name.to_vec()), String::from_utf8(desc.to_vec()));

					self.push_stack(2223)?;
				},
				Opcode::New => {
					let index = self.read_constant_pool_two_indexes()?;
					let name = if let CpInfo::Class(class) = self.class.get_constant_pool(index)? {
						class.name(&self.class)?
					} else {
						Err(WrongConstantPoolTag)?
					};

					println!("{:?}", String::from_utf8(name.to_vec()));
					self.push_stack(234)?;
				},
				Opcode::Dup => {
					let value = self.pop_stack()?;
					self.push_stack(value)?;
					self.push_stack(value)?;
				},
				Opcode::InvokeSpecial => {
					let index = self.read_constant_pool_two_indexes()?;

					let (class, name, desc) = if let CpInfo::MethodRef(method_ref) = self.class.get_constant_pool(index)? {
						method_ref.class_name_descriptor(&self.class)?
					} else {
						Err(WrongConstantPoolTag)?
					};

					println!("{:?}, {:?}, {:?}", String::from_utf8(class.to_vec()), String::from_utf8(name.to_vec()), String::from_utf8(desc.to_vec()));

					if desc == "()V" {
						self.pop_stack()?;
					}
				},
				Opcode::Ldc => {
					let index = self.read_isn()? as usize;
					println!("{:?}", self.class.get_constant_pool(index)?);
					self.push_stack(3444)?;
				},
				Opcode::InvokeVirtual => {
					let index = self.read_constant_pool_two_indexes()?;

					let (class, name, desc) = if let CpInfo::MethodRef(method_ref) = self.class.get_constant_pool(index)? {
						method_ref.class_name_descriptor(&self.class)?
					} else {
						Err(WrongConstantPoolTag)?
					};

					println!("{:?}, {:?}, {:?}", String::from_utf8(class.to_vec()), String::from_utf8(name.to_vec()), String::from_utf8(desc.to_vec()));

					let desc = desc;
					if desc == "(Ljava/lang/String;)Ljava/lang/StringBuilder;" {
						self.pop_stack()?;
						let this = self.pop_stack()?;
						self.push_stack(this)?;
					}

					if desc == "()Ljava/lang/String;" {
						let this = self.pop_stack()?;
						self.push_stack(this / 2)?;
					}

					if desc == "(Ljava/lang/String;)V" {
						self.pop_stack()?;
					}
				},
				Opcode::ALoad0 => {
					self.push_stack(444444)?;
				},
				Opcode::InvokeStatic => {
					let index = self.read_constant_pool_two_indexes()?;

					let (class, name, desc) = if let CpInfo::MethodRef(method_ref) = self.class.get_constant_pool(index)? {
						method_ref.class_name_descriptor(&self.class)?
					} else {
						Err(WrongConstantPoolTag)?
					};

					println!("{:?}, {:?}, {:?}", String::from_utf8(class.to_vec()), String::from_utf8(name.to_vec()), String::from_utf8(desc.to_vec()));

					if desc == "([Ljava/lang/Object;)Ljava/lang/String;" {
						let value = self.pop_stack()?;
						self.push_stack(value + 100000)?;
					}
				},
				Opcode::Return => {
					println!("done.");
					break;
				},
				_ => todo!("opcode {opcode:?} not yet implemented"),
			}

			println!("{self}");

			if self.program_counter >= self.code.len() {
				break;
			}
		}

		Ok(())
	}
}

#[cfg(test)]
mod testing {
	use std::fs::File;
	use std::io::BufReader;
	use zip::ZipArchive;
	use crate::classfile::{AttributeInfo, ClassFile, CpInfo, JUtf8, Parse};
	use crate::executor::Vm;
	use super::VmStackFrame;

	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_isn_from_real_class_file() {
		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		let classfile = ClassFile::parse(&mut &bytes[..], None).unwrap();
		classfile.verify().unwrap();

		println!("{:#?}", classfile);

		let method = classfile.methods.get(1).unwrap();
		let code = method.get_code().unwrap();

		if let AttributeInfo::Code {
			max_stack,
			max_locals,
			code_length,
			code,
			exception_table_length,
			exception_table,
			attributes_count,
			attributes
		} = code {
			let mut frame = VmStackFrame {
				program_counter: 0,
				stack_pointer: 0,
				code: code.to_owned(),
				local_variables: {
					let mut vec = Vec::with_capacity(*max_locals as usize);
					for _ in 0..(*max_locals as usize) { vec.push(0) }
					vec
				},
				operand_stack: {
					let mut vec = Vec::with_capacity(*max_stack as usize);
					for _ in 0..(*max_stack as usize) { vec.push(0) }
					vec
				},
				class: classfile.clone(),
			};

			println!("{}", frame);
			frame.run_isn().unwrap();
			println!("{}", frame);
		}
	}

	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_with_classes_from_rt_jar() {
		let rt = File::open("/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar").unwrap();
		let mut rt = ZipArchive::new(BufReader::new(rt)).unwrap();

		let mut vm = Vm::new();

		vm.try_add_class(rt.by_name("java/lang/Object.class").unwrap()).unwrap();

		vm.try_add_class(rt.by_name("java/lang/System.class").unwrap()).unwrap();

		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		vm.try_add_class(&bytes[..]).unwrap();

		let class = vm.get_class(&"Test".into()).unwrap();

		let main = class.methods.iter().find(|m| m.name(class).unwrap() == "main").unwrap();

		let code = main.get_code().unwrap();

		if let AttributeInfo::Code {
			max_stack,
			max_locals,
			code_length,
			code,
			exception_table_length,
			exception_table,
			attributes_count,
			attributes
		} = code {
			let frame = VmStackFrame {
				program_counter: 0,
				stack_pointer: 0,
				code: code.to_owned(),
				local_variables: {
					let mut vec = Vec::with_capacity(*max_locals as usize);
					for _ in 0..(*max_locals as usize) { vec.push(0) }
					vec
				},
				operand_stack: {
					let mut vec = Vec::with_capacity(*max_stack as usize);
					for _ in 0..(*max_stack as usize) { vec.push(0) }
					vec
				},
				class: class.clone(),
			};


			vm.stack.frames.push(frame);

			vm.start_at_current_frame().unwrap();
		}
	}


	#[test]
	fn test_class_instance() {
		let mut ci = ClassInstance {
			data: Box::new([0; 30]),
		};

		ci.put_int(10, 0x05060708).unwrap();
		ci.put_int(14, 0x67FFFFFF).unwrap();

		assert_eq!(ci.get_int(10).unwrap(), 0x05060708);
		assert_eq!(ci.get_int(14).unwrap(), 0x67FFFFFF);
	}
}

