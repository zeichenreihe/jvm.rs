use std::fmt::{Display, Formatter};
use std::mem;
use std::rc::Rc;
use crate::class_instance::Class;
use crate::class_loader::ClassLoader;
use crate::classfile::{ClassInfo, ConstantPoolElement, FieldRefInfo, MethodRefInfo};
use crate::code::Code;
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
pub type JReference = u32; //todo!();

pub const J_NULL: JReference = u32::MAX;


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
	code: Code,
	class: Rc<Class>,
	loader: ClassLoader,
}

impl Display for VmStackFrame {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "VmStackFrame: lvs: {:?}; stack: {:?}; sp: {:?}, pc: {:?}", self.local_variables, self.operand_stack, self.stack_pointer, self.program_counter)
	}
}

impl VmStackFrame {
	#[inline]
	fn push_stack(&mut self, value: StackFrameLvType) -> Result<(), OutOfBoundsError> {
		*self.operand_stack
			.get_mut(self.stack_pointer)
			.ok_or(OutOfBoundsError)? = value;
		self.stack_pointer += 1;
		Ok(())
	}

	#[inline]
	fn pop_stack(&mut self) -> Result<StackFrameLvType, OutOfBoundsError> {
		self.stack_pointer -= 1;
		Ok(mem::replace(
			self.operand_stack
				.get_mut(self.stack_pointer)
				.ok_or(OutOfBoundsError)?,
			StackFrameLvType::Empty
		))
	}

	#[inline]
	fn store_lv(&mut self, lv_index: usize, value: StackFrameLvType) -> Result<(), OutOfBoundsError> {
		*self.local_variables
			.get_mut(lv_index)
			.ok_or(OutOfBoundsError)? = value;
		Ok(())
	}

	#[inline]
	fn load_lv(&mut self, lv_index: usize) -> Result<StackFrameLvType, OutOfBoundsError> {
		self.local_variables
			.get(lv_index)
			.cloned()
			.ok_or(OutOfBoundsError)
	}

	fn next_isn(&mut self) -> Result<Opcode, OutOfBoundsError> {
		let opcode = self.code.code
			.get(self.program_counter)
			.ok_or(OutOfBoundsError)?
			.clone();
		self.program_counter += 1;
		Ok(opcode)
	}

	fn run_isn(&mut self) -> Result<(), RuntimeError> {
		println!("{self}");
		loop {
			let opcode = self.next_isn()?;

			println!("opcode: {opcode:?}");

			// TODO: make this actually allocate memory
			match opcode {
				Opcode::IConst0 => self.push_stack(StackFrameLvType::Int(0))?,
				Opcode::IConst1 => self.push_stack(StackFrameLvType::Int(1))?,
				Opcode::IConst2 => self.push_stack(StackFrameLvType::Int(2))?,
				Opcode::IConst3 => self.push_stack(StackFrameLvType::Int(3))?,
				Opcode::IConst4 => self.push_stack(StackFrameLvType::Int(4))?,
				Opcode::IConst5 => self.push_stack(StackFrameLvType::Int(5))?,
				Opcode::IConstM1 => self.push_stack(StackFrameLvType::Int(-1))?,
				Opcode::IAdd => {
					let a = self.pop_stack()?.try_as_int()?;
					let b = self.pop_stack()?.try_as_int()?;
					self.push_stack(StackFrameLvType::Int(a + b))?
				},

				Opcode::IStore1 => {
					let i = self.pop_stack()?;
					// TODO: verify: int
					self.store_lv(1, i)?;
				},
				Opcode::ILoad1 => {
					let i = self.load_lv(1)?;
					// TODO: verify: int
					self.push_stack(i)?;
				},

				Opcode::ALoad0 => {
					let a = self.load_lv(0)?;
					// TODO: verify: reference
					self.push_stack(a)?;
				},

				Opcode::ALoad1 => {
					let a = self.load_lv(1)?;
					// TODO: verify: reference
					self.push_stack(a)?;
				},
				Opcode::ALoad2 => {
					let a = self.load_lv(2)?;
					// TODO: verify: reference
					self.push_stack(a)?;
				},
				Opcode::ALoad3 => {
					let a = self.load_lv(3)?;
					// TODO: verify: reference
					self.push_stack(a)?;
				},

				Opcode::AStore1 => {
					let a = self.pop_stack()?;
					// TODO: verify: reference
					self.store_lv(1, a)?;
				},
				Opcode::AStore2 => {
					let a = self.pop_stack()?;
					// TODO: verify: reference
					self.store_lv(2, a)?;
				},
				Opcode::AStore3 => {
					let a = self.pop_stack()?;
					// TODO: verify: reference
					self.store_lv(3, a)?;
				}


				Opcode::IfICmpGe { branch_target } => {
					let value2 = self.pop_stack()?.try_as_int()?;
					let value1 = self.pop_stack()?.try_as_int()?;
					if value1 >= value2 {
						let old_pc = self.program_counter;
						self.program_counter = branch_target;
						println!("jumped from {} to {}", old_pc, self.program_counter);
					}
				},
				Opcode::GetStatic { cp_index } => {
					let index = cp_index as usize;
					let field_ref: FieldRefInfo = self.class.class.constant_pool.get(index)?;

					let class = self.loader.get(&field_ref.class)?;

					let field = class.static_fields.get(&field_ref.name_and_type).unwrap();
					// TODO: unwrap, also doesn't impl the whole field resolution procedure

					let to_stack = field.load(class)?;

					println!("{} {} {} => {:?}", &field_ref.class, &field_ref.name_and_type.name, &field_ref.name_and_type.descriptor, &to_stack);

					self.push_stack(to_stack)?;
				},
				Opcode::New { cp_index } => {
					let index = cp_index as usize;
					let name: ClassInfo = self.class.class.constant_pool.get(index)?;

					println!("{name:?}");
					self.push_stack(StackFrameLvType::Reference(234))?;
				},
				Opcode::NewArray { a_type } => {
					let _count = self.pop_stack()?;
					println!("{a_type:?}");
					self.push_stack(StackFrameLvType::Reference(2333334))?;
				},
				Opcode::BIPush { byte } => {
					self.push_stack(StackFrameLvType::Int(byte as i32))?;
				}
				Opcode::CAStore => {
					let value = self.pop_stack()?;
					let index = self.pop_stack()?;
					let arrayref = self.pop_stack()?;
					println!("storing: {value:?} at {index:?}");
				}
				Opcode::Dup => {
					let value = self.pop_stack()?;
					self.push_stack(value)?;
					self.push_stack(value)?;
				},
				Opcode::InvokeSpecial { cp_index } => {
					let index = cp_index as usize;
					let method_ref: MethodRefInfo = self.class.class.constant_pool.get(index)?;

					let class = method_ref.class;
					let name = method_ref.name_and_type.name;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					let desc = desc.to_string();
					if desc == String::from("()V") {
						self.pop_stack()?;
					}
					if desc == "([C)V" {
						self.pop_stack()?;
						self.pop_stack()?;
					}
				},
				Opcode::Ldc { cp_index } => {
					let index = cp_index as usize;
					let item: ConstantPoolElement = self.class.class.constant_pool.get(index)?;
					println!("{item:?}");
					self.push_stack(StackFrameLvType::Reference(3444))?;
				},
				Opcode::InvokeVirtual { cp_index } => {
					let index = cp_index as usize;
					let method_ref: MethodRefInfo = self.class.class.constant_pool.get(index)?;

					let class = method_ref.class;
					let name = method_ref.name_and_type.name;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					let class = class.to_string();
					let name = name.to_string();
					let desc = desc.to_string();

					if desc == "(Ljava/lang/String;)Ljava/lang/StringBuilder;" {
						let _string = self.pop_stack();
					}
					if desc == "(Ljava/lang/String;)V" {
						let _string = self.pop_stack()?;
					}
					if desc == "(I)Ljava/lang/StringBuilder;" {
						let _int = self.pop_stack()?;
					}
					if desc == "(I)V" {
						let _int = self.pop_stack()?;
					}

					let _this = self.pop_stack()?;

					if desc == "(I)Ljava/lang/StringBuilder;" {
						self.push_stack(StackFrameLvType::Reference(290598025))?;
					}
					if desc == "()Ljava/lang/String;" {
						self.push_stack(StackFrameLvType::Reference(890225890))?;
					}
					if desc == "(Ljava/lang/String;)Ljava/lang/StringBuilder;" {
						self.push_stack(StackFrameLvType::Reference(290598025))?;
					}
					if desc == "()I" {
						self.push_stack(StackFrameLvType::Int(666))?;
					}
				},
				Opcode::InvokeStatic { cp_index } => {
					let index = cp_index as usize;
					let method_ref: MethodRefInfo = self.class.class.constant_pool.get(index)?;

					let class = method_ref.class;
					let name = method_ref.name_and_type.name;
					let desc = method_ref.name_and_type.descriptor;

					println!("{class}, {name}, {desc}");

					let desc = desc.to_string();
					if desc == "([Ljava/lang/Object;)Ljava/lang/String;" {
						let _value = self.pop_stack()?;
						self.push_stack(StackFrameLvType::Reference(2235890))?;
					}
					if desc == "(Ljava/lang/Object;)I" {
						let _value = self.pop_stack()?;
						self.push_stack(StackFrameLvType::Int(6666))?;
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

			if self.program_counter >= self.code.code.len() {
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
	use crate::class_loader::{ClassesSource, ClassLoader};
	use crate::classfile::ClassInfo;
	use crate::executor::{StackFrameLvType};
	use super::VmStackFrame;

/*	#[test]
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
				non_static_fields: HashMap::new(),
				static_fields: HashMap::new(),

				static_data: Vec::new(),
			},
		};

		println!("{}", frame);
		frame.run_isn().unwrap();
		println!("{}", frame);

		let _ = frame;
	}*/

	fn read_as_vec<R: Read>(mut reader: R) -> Vec<u8> {
		let mut vec = Vec::new();
		reader.read_to_end(&mut vec).unwrap();
		vec
	}

	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_with_classes_from_rt_jar() {
		if true { return }

		let rt = File::open("/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar").unwrap();
		let mut rt = ZipArchive::new(BufReader::new(rt)).unwrap();

		let mut loader = ClassLoader::new();
		loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("java/lang/Object"),
				bytes: read_as_vec(rt.by_name("java/lang/Object.class").unwrap()),
			},
		);

		loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("java/lang/System"),
				bytes: read_as_vec(rt.by_name("java/lang/System.class").unwrap())
			},
		);

		loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("Test"),
				bytes: include_bytes!("../../java_example_classfiles/Test.class").to_vec(),
			},
		);

		let class = loader.get(&ClassInfo::from("Test")).unwrap();

		let main = class.class.methods.iter()
			.find(|m| m.name.to_string().as_str() == "main")
			.unwrap();

		let code = main.code.as_ref().unwrap();
		let stack_map_frame = &code.stack_map_table;

		dbg!(stack_map_frame);

		let mut frame = VmStackFrame {
			program_counter: 0,
			stack_pointer: 0,
			code: code.code.clone(),
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
			loader,
		};

		frame.run_isn().unwrap();
	}


	#[test]
	#[cfg(target_os = "linux")]
	fn test_run_with_classes_from_rt_jar_test_2() {
		let rt = File::open("/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar").unwrap();
		let mut rt = ZipArchive::new(BufReader::new(rt)).unwrap();

		let mut loader = ClassLoader::new();
		loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("java/lang/Object"),
				bytes: read_as_vec(rt.by_name("java/lang/Object.class").unwrap()),
			},
		);

		loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("java/lang/System"),
				bytes: read_as_vec(rt.by_name("java/lang/System.class").unwrap())
			},
		);

		loader.sources.push(
			ClassesSource::Bytes {
				name: String::from("Test2"),
				bytes: include_bytes!("../../java_example_classfiles/Test2.class").to_vec(),
			},
		);

		dbg!(loader.get(&ClassInfo::from("java/lang/System"))).unwrap();

		let class = loader.get(&ClassInfo::from("Test2")).unwrap();

		let main = class.class.methods.iter()
			.find(|m| m.name.to_string().as_str() == "main")
			.unwrap();

		let code = main.code.as_ref().unwrap();
		let stack_map_frame = &code.stack_map_table;

		dbg!(code);
		dbg!(stack_map_frame);

		let mut frame = VmStackFrame {
			program_counter: 0,
			stack_pointer: 0,
			code: code.code.clone(),
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
			loader,
		};

		frame.run_isn().unwrap();
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

