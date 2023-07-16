use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use std::mem::size_of;
use std::sync::Arc;
use crate::classfile::{ClassFile, ConstantPoolElement, FieldRefInfo, JUtf8, MethodRefInfo, Utf8Info};
use crate::errors::{ClassFileParseError, ClassLoadError, OutOfBoundsError, RuntimeError};
use crate::opcodes::Opcode;

pub type JBoolean = bool;//todo!();
pub type JByte = i8;
pub type JShort = i16;
pub type JChar = char;//todo!();
pub type JInt = i32;
pub type JLong = i64;
pub type JFloat = f32;
pub type JDouble = f64;
pub type JReference = u64; //todo!();


#[derive(Debug)]
struct Vm {
	classes: HashMap<JUtf8, ClassFile>,
	stack: VmStack,
}

impl Vm {
	fn new() -> Vm {
		Vm { classes: HashMap::new(), stack: VmStack { frames: Vec::new() }}
	}

	fn try_add_class<R: Read>(&mut self, mut reader: R) -> Result<(), ClassLoadError> {
		let classfile = ClassFile::parse(&mut reader)?;

		match self.classes.insert(classfile.this_class.name.clone().into(), classfile) {
			Some(class) => Err(ClassLoadError::ClassAlreadyLoaded(class.this_class.name.into())),
			None => Ok(()),
		}
	}

	fn get_class(&self, name: &JUtf8) -> Result<&ClassFile, ClassLoadError> {
		self.classes.get(name).ok_or_else(|| ClassLoadError::NoClassDefFound(name.to_owned()))
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
	fn push_stack(&mut self, value: u32) -> Result<(), OutOfBoundsError> {
		*self.operand_stack
			.get_mut(self.stack_pointer)
			.ok_or(OutOfBoundsError)? = value;
		self.stack_pointer += 1;
		Ok(())
	}

	fn pop_stack(&mut self) -> Result<u32, OutOfBoundsError> {
		self.stack_pointer -= 1;
		self.operand_stack
			.get(self.stack_pointer)
			.ok_or(OutOfBoundsError)
			.map(|i| *i)
	}

	fn read_isn(&mut self) -> Result<u8, OutOfBoundsError> {
		let value = self.code
			.get(self.program_counter)
			.ok_or(OutOfBoundsError)
			.map(|i| *i);
		self.program_counter += 1;
		value
	}

	fn read_constant_pool_two_indexes(&mut self) -> Result<usize, RuntimeError> {
		let index_byte1 = self.read_isn()? as usize;
		let index_byte2 = self.read_isn()? as usize;
		Ok(index_byte1 << 8 | index_byte2)
	}

	fn run_isn(&mut self) -> Result<(), RuntimeError> {
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
					let field_ref: FieldRefInfo = self.class.constant_pool.get(index)?;

					let name = field_ref.name_and_type.name;
					let class = field_ref.class;
					let desc = field_ref.name_and_type.descriptor;

					println!("{class:?}, {name:?}, {desc:?}");

					self.push_stack(2223)?;
				},
				Opcode::New => {
					let index = self.read_constant_pool_two_indexes()?;
					let name: Utf8Info = self.class.constant_pool.get(index)?;

					println!("{name:?}");
					self.push_stack(234)?;
				},
				Opcode::Dup => {
					let value = self.pop_stack()?;
					self.push_stack(value)?;
					self.push_stack(value)?;
				},
				Opcode::InvokeSpecial => {
					let index = self.read_constant_pool_two_indexes()?;

					let method_ref: MethodRefInfo = self.class.constant_pool.get(index)?;

					let name = method_ref.name_and_type.name;
					let class = method_ref.class;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class:?}, {name:?}, {desc:?}");

					let desc = desc.to_string();
					if desc == String::from("()V") {
						self.pop_stack()?;
					}
				},
				Opcode::Ldc => {
					let index = self.read_isn()? as usize;
					let item: ConstantPoolElement = self.class.constant_pool.get(index)?;
					println!("{item:?}");
					self.push_stack(3444)?;
				},
				Opcode::InvokeVirtual => {
					let index = self.read_constant_pool_two_indexes()?;

					let method_ref: MethodRefInfo = self.class.constant_pool.get(index)?;

					let name = method_ref.name_and_type.name;
					let class = method_ref.class;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class:?}, {name:?}, {desc:?}");

					let desc = desc.to_string();
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

					let method_ref: MethodRefInfo = self.class.constant_pool.get(index)?;

					let name = method_ref.name_and_type.name;
					let class = method_ref.class;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class:?}, {name:?}, {desc:?}");

					let desc = desc.to_string();
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
	use std::sync::atomic::AtomicUsize;
	use std::sync::atomic::Ordering::Relaxed;
	use inkwell::module::Linkage;
	use zip::ZipArchive;
	use crate::classfile::{AttributeInfo, ClassFile, CodeAttribute};
	use crate::executor::Vm;
	use super::VmStackFrame;

	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_isn_from_real_class_file() {
		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		let classfile = ClassFile::parse(&mut &bytes[..]).unwrap();
		classfile.verify().unwrap();

		let method = classfile.methods.get(1).unwrap();
		let code = method.get_code().unwrap();

		if let AttributeInfo::Code(CodeAttribute {
			max_stack,
			max_locals,
			code,
			exception_table: _,
			attributes: _,
			..
		}) = code {
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

			//println!("{}", frame);
			//frame.run_isn().unwrap();
			//println!("{}", frame);

			let _ = frame;
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

		let main = class.methods.iter()
			.find(|m| m.name.to_string().as_str() == "main")
			.unwrap();

		let code = main.get_code().unwrap();

		if let AttributeInfo::Code(CodeAttribute {
			max_stack,
			max_locals,
			code,
			exception_table: _,
			attributes: _
		}) = code {
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

			vm.stack.frames.last_mut().unwrap().run_isn().unwrap();
		}
	}

	#[test]
	fn test_jit() {
		use inkwell::OptimizationLevel;
		use inkwell::context::Context;
		use inkwell::execution_engine::JitFunction;

		static CALLS: AtomicUsize = AtomicUsize::new(0);
		#[no_mangle]
		pub extern "C" fn do_fun_stuff(a: u64) -> u64 {
			CALLS.fetch_add(1, Relaxed);
			a * 3
		}

		let context = Context::create();

		{ // add functions to the jit access table
			let name = std::ffi::CString::new("do_fun_stuff").unwrap();
			let value = do_fun_stuff as *mut std::ffi::c_void;
			unsafe { llvm_sys::support::LLVMAddSymbol(name.as_ptr(), value) }
		}

		let module = context.create_module("sum");
		{
			let i64_type = context.i64_type();
			let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);

			let fun_fn_type = i64_type.fn_type(&[i64_type.into()], false);
			let do_fun_stuff = module.add_function("do_fun_stuff", fun_fn_type, Some(Linkage::External));
			{
				let builder = context.create_builder();
				let function = module.add_function("sum/bar foo () baz", fn_type, None);
				let basic_block = context.append_basic_block(function, "entry");

				builder.position_at_end(basic_block);

				let x = function.get_nth_param(0).unwrap().into_int_value();
				let y = function.get_nth_param(1).unwrap().into_int_value();
				let z = function.get_nth_param(2).unwrap().into_int_value();

				let x = builder.build_direct_call(do_fun_stuff, &[x.into()], "")
					.try_as_basic_value()
					.left().unwrap().into_int_value();
				let y = builder.build_direct_call(do_fun_stuff, &[y.into()], "y")
					.try_as_basic_value()
					.left().unwrap().into_int_value();
				let z = builder.build_direct_call(do_fun_stuff, &[z.into()], "y")
					.try_as_basic_value()
					.left().unwrap().into_int_value();


				let sum = builder.build_int_mul(x, y, "mul");
				let sum = builder.build_int_mul(sum, z, "mul");

				builder.build_return(Some(&sum));
			}
		}
		module.verify().unwrap();
		let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

		let sum: JitFunction<unsafe extern "C" fn(u64, u64, u64) -> u64> = unsafe { execution_engine.get_function("sum/bar foo () baz") }.unwrap();

		let x = 3u64;
		let y = 2u64;
		let z = 5u64;

		let sum = unsafe { sum.call(x, y, z) };

		assert_eq!(CALLS.load(Relaxed), 3);

		let x = do_fun_stuff(x);
		let y = do_fun_stuff(y);
		let z = do_fun_stuff(z);

		assert_eq!(sum, x * y * z);
	}

/*
	#[test]
	fn test_class_instance() {
		let mut ci = ClassInstance {
			class: ,//todo!(),
			data: Box::new([0; 30]),
		};

		ci.put_int(10, 0x05060708).unwrap();
		ci.put_int(14, 0x67FFFFFF).unwrap();

		assert_eq!(ci.get_int(10).unwrap(), 0x05060708);
		assert_eq!(ci.get_int(14).unwrap(), 0x67FFFFFF);

		let _ = ci;
	}*/
}

