use std::fmt::{Display, Formatter};
use std::mem;
use crate::class_instance::Class;
use crate::class_loader::ClassLoader;
use crate::classfile::{ClassInfo, ConstantPoolElement, FieldRefInfo, MethodRefInfo};
use crate::errors::{OutOfBoundsError, RuntimeError};
use crate::opcodes::Opcode;

#[derive(Debug, Clone, Copy)]
pub enum StackFrameLvType {
	Empty,
	Boolean(JBoolean), Byte(JByte), Short(JShort), Char(JChar), Int(JInt), Long(JLong), Float(JFloat), Double(JDouble), Reference(JReference)
}
impl StackFrameLvType {
	fn try_as_int(&self) -> Result<JInt, RuntimeError> {
		if let Self::Int(int) = self {
			Ok(int.clone())
		} else {
			Err(RuntimeError::TypeMismatch)
		}
	}
}

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
	loader: ClassLoader,
	stack: VmStack,
}

impl Vm {
	fn new() -> Vm {
		Vm {
			loader: ClassLoader::new(),
			stack: VmStack { frames: Vec::new() }
		}
	}
}

#[derive(Debug)]
struct VmStack {
	frames: Vec<VmStackFrame>,
}

#[derive(Debug)]
struct VmStackFrame {
	local_variables: Vec<StackFrameLvType>,
	operand_stack: Vec<StackFrameLvType>,
	stack_pointer: usize,

	program_counter: usize,
	code: Vec<u8>,
	class: Class,
}

impl Display for VmStackFrame {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "VmStackFrame: lvs: {:?}; stack: {:?}; sp: {:?}, pc: {:?}", self.local_variables, self.operand_stack, self.stack_pointer, self.program_counter)
	}
}

impl VmStackFrame {
	fn push_stack(&mut self, value: StackFrameLvType) -> Result<(), OutOfBoundsError> {
		*self.operand_stack
			.get_mut(self.stack_pointer)
			.ok_or(OutOfBoundsError)? = value;
		self.stack_pointer += 1;
		Ok(())
	}

	fn pop_stack(&mut self) -> Result<StackFrameLvType, OutOfBoundsError> {
		self.stack_pointer -= 1;
		Ok(mem::replace(
			self.operand_stack
				.get_mut(self.stack_pointer)
				.ok_or(OutOfBoundsError)?,
			StackFrameLvType::Empty
		))
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
				Opcode::IConst0 => self.push_stack(StackFrameLvType::Int(0))?,
				Opcode::IConst1 => self.push_stack(StackFrameLvType::Int(1))?,
				Opcode::IConst2 => self.push_stack(StackFrameLvType::Int(2))?,
				Opcode::IConst3 => self.push_stack(StackFrameLvType::Int(3))?,
				Opcode::IAdd => {
					let a = self.pop_stack()?.try_as_int()?;
					let b = self.pop_stack()?.try_as_int()?;
					self.push_stack(StackFrameLvType::Int(a + b))?
				},
				Opcode::GetStatic => {
					let index = self.read_constant_pool_two_indexes()?;
					let field_ref: FieldRefInfo = self.class.class.constant_pool.get(index)?;

					let class = field_ref.class;
					let name = field_ref.name_and_type.name;
					let desc = field_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					self.push_stack(StackFrameLvType::Reference(2223))?;
				},
				Opcode::New => {
					let index = self.read_constant_pool_two_indexes()?;
					let name: ClassInfo = self.class.class.constant_pool.get(index)?;

					println!("{name:?}");
					self.push_stack(StackFrameLvType::Reference(234))?;
				},
				Opcode::Dup => {
					let value = self.pop_stack()?;
					self.push_stack(value)?;
					self.push_stack(value)?;
				},
				Opcode::InvokeSpecial => {
					let index = self.read_constant_pool_two_indexes()?;
					let method_ref: MethodRefInfo = self.class.class.constant_pool.get(index)?;

					let class = method_ref.class;
					let name = method_ref.name_and_type.name;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					let desc = desc.to_string();
					if desc == String::from("()V") {
						self.pop_stack()?;
					}
				},
				Opcode::Ldc => {
					let index = self.read_isn()? as usize;
					let item: ConstantPoolElement = self.class.class.constant_pool.get(index)?;
					println!("{item:?}");
					self.push_stack(StackFrameLvType::Reference(3444))?;
				},
				Opcode::InvokeVirtual => {
					let index = self.read_constant_pool_two_indexes()?;
					let method_ref: MethodRefInfo = self.class.class.constant_pool.get(index)?;

					let class = method_ref.class;
					let name = method_ref.name_and_type.name;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					let desc = desc.to_string();
					if desc == "(Ljava/lang/String;)Ljava/lang/StringBuilder;" {
						self.pop_stack()?;
						let this = self.pop_stack()?;
						self.push_stack(StackFrameLvType::Reference(290598025))?;
					}

					if desc == "()Ljava/lang/String;" {
						let this = self.pop_stack()?;
						self.push_stack(StackFrameLvType::Reference(890225890))?;
					}

					if desc == "(Ljava/lang/String;)V" {
						self.pop_stack()?;
					}
				},
				Opcode::ALoad0 => {
					self.push_stack(StackFrameLvType::Reference(444444))?;
				},
				Opcode::InvokeStatic => {
					let index = self.read_constant_pool_two_indexes()?;
					let method_ref: MethodRefInfo = self.class.class.constant_pool.get(index)?;

					let class = method_ref.class;
					let name = method_ref.name_and_type.name;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					let desc = desc.to_string();
					if desc == "([Ljava/lang/Object;)Ljava/lang/String;" {
						let value = self.pop_stack()?;
						self.push_stack(StackFrameLvType::Reference(2235890))?;
					}
				},
				Opcode::Return => {
					println!("done.");
					break;
				},
				Opcode::Nop => {},
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
	use std::io::{BufReader, Read};
	use std::sync::atomic::AtomicUsize;
	use std::sync::atomic::Ordering::Relaxed;
	use inkwell::module::Linkage;
	use zip::ZipArchive;
	use crate::class_instance::{Class, Field};
	use crate::class_loader::ClassesSource;
	use crate::classfile::{AttributeInfo, ClassFile, ClassInfo, CodeAttribute};
	use crate::executor::{StackFrameLvType, Vm};
	use crate::types::descriptor::{BaseOrObjectType, FieldDescriptor};
	use super::VmStackFrame;

	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_isn_from_real_class_file() {
		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		let class_file = ClassFile::parse(&mut &bytes[..]).unwrap();
		class_file.verify().unwrap();

		let method = class_file.methods.get(1).unwrap();
		let code = method.code.as_ref().unwrap();

		let mut frame = VmStackFrame {
			program_counter: 0,
			stack_pointer: 0,
			code: code.code.to_owned(),
			local_variables: {
				let mut vec = Vec::with_capacity(code.max_locals as usize);
				for _ in 0..(code.max_locals as usize) { vec.push(StackFrameLvType::Empty) }
				vec
			},
			operand_stack: {
				let mut vec = Vec::with_capacity(code.max_stack as usize);
				for _ in 0..(code.max_stack as usize) { vec.push(StackFrameLvType::Empty) }
				vec
			},
			class: Class {
				super_class_size: 0,
				class_size: 0,
				class: class_file.clone(),
				fields: class_file.fields.iter()
					.map(|f| {
						Field {
							size: 0,
							field_offset: 0,
							descriptor: FieldDescriptor {
								array_dimension: 0,
								base_or_object_type: BaseOrObjectType::B
							}
						}
					})
					.collect(),
			},
		};

		println!("{}", frame);
		frame.run_isn().unwrap();
		println!("{}", frame);

		let _ = frame;
	}

	fn read_as_vec<R: Read>(mut reader: R) -> Vec<u8> {
		let mut vec = Vec::new();
		reader.read_to_end(&mut vec).unwrap();
		vec
	}

	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_with_classes_from_rt_jar() {
		let rt = File::open("/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar").unwrap();
		let mut rt = ZipArchive::new(BufReader::new(rt)).unwrap();

		let mut vm = Vm::new();

		vm.loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("java/lang/Object"),
				bytes: read_as_vec(rt.by_name("java/lang/Object.class").unwrap()),
			},
		);

		vm.loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("java/lang/System"),
				bytes: read_as_vec(rt.by_name("java/lang/System.class").unwrap())
			},
		);

		vm.loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("Test"),
				bytes: include_bytes!("../../java_example_classfiles/Test.class").to_vec(),
			},
		);

		vm.loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("Test2"),
				bytes: include_bytes!("../../java_example_classfiles/Test2.class").to_vec(),
			},
		);

		let class = vm.loader.get(&ClassInfo::from("Test")).unwrap();

		let main = class.class.methods.iter()
			.find(|m| m.name.to_string().as_str() == "main")
			.unwrap();

		let code = main.code.as_ref().unwrap();
		let stack_map_frame = &code.stack_map_table;

		dbg!(stack_map_frame);

		let frame = VmStackFrame {
			program_counter: 0,
			stack_pointer: 0,
			code: code.code.to_owned(),
			local_variables: {
				let mut vec = Vec::with_capacity(code.max_locals as usize);
				for _ in 0..(code.max_locals as usize) { vec.push(StackFrameLvType::Empty) }
				vec
			},
			operand_stack: {
				let mut vec = Vec::with_capacity(code.max_stack as usize);
				for _ in 0..(code.max_stack as usize) { vec.push(StackFrameLvType::Empty) }
				vec
			},
			class: class.clone(),
		};


		vm.stack.frames.push(frame);

		vm.stack.frames.last_mut().unwrap().run_isn().unwrap();

		let class = vm.loader.get(&ClassInfo::from("Test2")).unwrap();
		dbg!(class);
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

