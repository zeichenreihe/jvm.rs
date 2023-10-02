use std::fmt::{Debug, Formatter};
use std::mem::size_of;
use crate::classfile::cp::attribute::{ExceptionTableEntry, StackMapTableAttribute};
use crate::classfile::cp::{Pool, PoolEntry};
use crate::errors::{ClassFileParseError, OutOfBoundsError};
use crate::classfile::instruction::{LvIndex, Opcode};

struct CodeReader {
	bytes: Vec<u8>,
	pos: usize,
	instruction_count: usize,
	this_instruction_pos: usize,
}

impl CodeReader {
	fn new(bytes: Vec<u8>) -> CodeReader {
		CodeReader {
			bytes,
			pos: 0,
			instruction_count: 0,
			this_instruction_pos: 0,
		}
	}

	fn has_elements(&self) -> bool {
		self.pos < self.bytes.len()
	}

	fn next_instruction(&mut self) -> () {
		self.instruction_count += 1;
		self.this_instruction_pos = self.pos;
	}

	fn move_to_next_4_byte_boundary(&mut self) -> () {
		let pad_length = match self.pos % 4 {
			0 => 0,
			1 => 3,
			2 => 2,
			3 => 1,
			_ => unreachable!(),
		};
		self.pos += pad_length;
	}

	fn get_u8(&mut self) -> Result<u8, OutOfBoundsError> {
		let ret = self.bytes
			.get(self.pos).ok_or(OutOfBoundsError)?;
		self.pos += 1;
		Ok(*ret)
	}

	fn get_i16(&mut self) -> Result<i16, OutOfBoundsError> {
		let size = size_of::<i16>();
		let slice = self.bytes
			.get(self.pos..).ok_or(OutOfBoundsError)?
			.get(..size).ok_or(OutOfBoundsError)?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		self.pos += size;
		Ok(i16::from_be_bytes(slice))
	}

	fn get_u16(&mut self) -> Result<u16, OutOfBoundsError> {
		let size = size_of::<u16>();
		let slice = self.bytes
			.get(self.pos..).ok_or(OutOfBoundsError)?
			.get(..size).ok_or(OutOfBoundsError)?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		self.pos += size;
		Ok(u16::from_be_bytes(slice))
	}

	fn get_u8_as_usize(&mut self) -> Result<usize, OutOfBoundsError> {
		Ok(self.get_u8()? as usize) // TODO: can this panic?
	}

	fn get_u16_as_usize(&mut self) -> Result<usize, OutOfBoundsError> {
		Ok(self.get_u16()? as usize) // TODO: can this panic?
	}

	fn get_i32(&mut self) -> Result<i32, OutOfBoundsError> {
		let size = size_of::<i16>();
		let slice = self.bytes
			.get(self.pos..).ok_or(OutOfBoundsError)?
			.get(..size).ok_or(OutOfBoundsError)?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		self.pos += size;
		Ok(i32::from_be_bytes(slice))
	}

	fn get_i16_branchoffset(&mut self) -> Result<usize, OutOfBoundsError> {
		let offset = self.get_i16()?;
		let target = (offset as i32) + (self.this_instruction_pos as i32);

		Ok(target.try_into().unwrap())
	}

	fn get_i32_branchoffset(&mut self) -> Result<usize, OutOfBoundsError> {
		let offset = self.get_i32()?;
		let target = offset + (self.this_instruction_pos as i32);

		Ok(target.try_into().unwrap())
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct Code {
	pub code: Vec<Opcode>,
}

impl Debug for Code {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let mut t = f.debug_struct("Code");
		for (pos, entry) in self.code.iter().enumerate() {
			t.field(&format!("- {pos:?}"), &entry);
		}
		t.finish()
	}
}

impl Code {
	fn read_next_opcode(reader: &mut CodeReader, pool: &Pool) -> Result<Opcode, ClassFileParseError> {
		Ok(match reader.get_u8()? {
			0x32 => Opcode::AALoad,
			0x53 => Opcode::AAStore,
			0x01 => Opcode::AConstNull,
			0x19 => Opcode::ALoad(LvIndex(reader.get_u8_as_usize()?)),
			0x2a => Opcode::ALoad(LvIndex(0)),
			0x2b => Opcode::ALoad(LvIndex(1)),
			0x2c => Opcode::ALoad(LvIndex(2)),
			0x2d => Opcode::ALoad(LvIndex(3)),
			0xbd => Opcode::ANewArray {
				class: pool.get(reader.get_u16_as_usize()?)?
			},
			0xb0 => Opcode::AReturn,
			0xbe => Opcode::ArrayLength,
			0x3a => Opcode::AStore(LvIndex(reader.get_u8_as_usize()?)),
			0x4b => Opcode::AStore(LvIndex(0)),
			0x4c => Opcode::AStore(LvIndex(1)),
			0x4d => Opcode::AStore(LvIndex(2)),
			0x4e => Opcode::AStore(LvIndex(3)),
			0xbf => Opcode::AThrow,
			0x33 => Opcode::BALoad,
			0x54 => Opcode::BAStore,
			0x10 => Opcode::BIPush {
				byte: reader.get_u8()?,
			},
			0xca => Opcode::Breakpoint,
			0x34 => Opcode::CALoad,
			0x55 => Opcode::CAStore,
			0xc0 => Opcode::CheckCast {
				class: pool.get(reader.get_u16_as_usize()?)?,
			},
			0x90 => Opcode::D2f,
			0x8e => Opcode::D2i,
			0x8f => Opcode::D2l,
			0x63 => Opcode::DAdd,
			0x31 => Opcode::DALoad,
			0x52 => Opcode::DAStore,
			0x98 => Opcode::DCmpG,
			0x97 => Opcode::DCmpL,
			0x0e => Opcode::DConst0,
			0x0f => Opcode::DConst1,
			0x6f => Opcode::DDiv,
			0x18 => Opcode::DLoad(LvIndex(reader.get_u8_as_usize()?)),
			0x26 => Opcode::DLoad(LvIndex(0)),
			0x27 => Opcode::DLoad(LvIndex(1)),
			0x28 => Opcode::DLoad(LvIndex(2)),
			0x29 => Opcode::DLoad(LvIndex(3)),
			0x6b => Opcode::DMul,
			0x77 => Opcode::DNeg,
			0x73 => Opcode::DRem,
			0xaf => Opcode::DReturn,
			0x39 => Opcode::DStore(LvIndex(reader.get_u8_as_usize()?)),
			0x47 => Opcode::DStore(LvIndex(0)),
			0x48 => Opcode::DStore(LvIndex(1)),
			0x49 => Opcode::DStore(LvIndex(2)),
			0x4a => Opcode::DStore(LvIndex(3)),
			0x67 => Opcode::DSub,
			0x59 => Opcode::Dup,
			0x5a => Opcode::DupX1,
			0x5b => Opcode::DupX2,
			0x5c => Opcode::Dup2,
			0x5d => Opcode::Dup2X1,
			0x5e => Opcode::Dup2X2,
			0x8d => Opcode::F2d,
			0x8b => Opcode::F2i,
			0x8c => Opcode::F2l,
			0x62 => Opcode::FAdd,
			0x30 => Opcode::FALoad,
			0x51 => Opcode::FAStore,
			0x96 => Opcode::FCmpG,
			0x95 => Opcode::FCmpL,
			0x0b => Opcode::FConst0,
			0x0c => Opcode::FConst1,
			0x0d => Opcode::FConst2,
			0x6e => Opcode::FDiv,
			0x17 => Opcode::FLoad(LvIndex(reader.get_u8_as_usize()?)),
			0x22 => Opcode::FLoad(LvIndex(0)),
			0x23 => Opcode::FLoad(LvIndex(1)),
			0x24 => Opcode::FLoad(LvIndex(2)),
			0x25 => Opcode::FLoad(LvIndex(3)),
			0x6a => Opcode::FMul,
			0x76 => Opcode::FNeg,
			0x72 => Opcode::FRem,
			0xae => Opcode::FReturn,
			0x38 => Opcode::FStore(LvIndex(reader.get_u8_as_usize()?)),
			0x43 => Opcode::FStore(LvIndex(0)),
			0x44 => Opcode::FStore(LvIndex(1)),
			0x45 => Opcode::FStore(LvIndex(2)),
			0x46 => Opcode::FStore(LvIndex(3)),
			0x66 => Opcode::FSub,
			0xb4 => Opcode::GetField(pool.get(reader.get_u16_as_usize()?)?),
			0xb2 => Opcode::GetStatic(pool.get(reader.get_u16_as_usize()?)?),
			0xa7 => {
				let branch_target = reader.get_i16_branchoffset()?;
				Opcode::Goto { branch_target }
			},
			0xc8 => {
				let branch_target = reader.get_i32_branchoffset()?;
				Opcode::Goto { branch_target }
			},
			0x91 => Opcode::I2b,
			0x92 => Opcode::I2c,
			0x87 => Opcode::I2d,
			0x86 => Opcode::I2f,
			0x85 => Opcode::I2l,
			0x93 => Opcode::I2s,
			0x60 => Opcode::IAdd,
			0x2e => Opcode::IALoad,
			0x7e => Opcode::IAnd,
			0x4f => Opcode::IAStore,
			0x02 => Opcode::IConstM1,
			0x03 => Opcode::IConst0,
			0x04 => Opcode::IConst1,
			0x05 => Opcode::IConst2,
			0x06 => Opcode::IConst3,
			0x07 => Opcode::IConst4,
			0x08 => Opcode::IConst5,
			0x6c => Opcode::IDiv,
			0xa5 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfACmpEq { branch_target, else_branch_target }
			},
			0xa6 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfACmpNe { branch_target, else_branch_target }
			},
			0x9f => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfICmpEq { branch_target, else_branch_target }
			},
			0xa2 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfICmpGe { branch_target, else_branch_target }
			},
			0xa3 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfICmpGt { branch_target, else_branch_target }
			},
			0xa4 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfICmpLe { branch_target, else_branch_target }
			},
			0xa1 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfICmpLt { branch_target, else_branch_target }
			},
			0xa0 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfICmpNe { branch_target, else_branch_target }
			},
			0x99 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfEq { branch_target, else_branch_target }
			},
			0x9c => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfGe { branch_target, else_branch_target }
			},
			0x9d => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfGt { branch_target, else_branch_target }
			},
			0x9e => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfLe { branch_target, else_branch_target }
			},
			0x9b => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfLt { branch_target, else_branch_target }
			},
			0x9a => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfNe { branch_target, else_branch_target }
			},
			0xc7 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfNonNull { branch_target, else_branch_target }
			},
			0xc6 => {
				let branch_target = reader.get_i16_branchoffset()?;
				let else_branch_target = reader.pos;
				Opcode::IfNull { branch_target, else_branch_target }
			},
			0x84 => Opcode::IInc {
				lv_index: reader.get_u8_as_usize()?,
				const_: reader.get_u8()? as i32,
			},
			0x15 => Opcode::ILoad(LvIndex(reader.get_u8_as_usize()?)),
			0x1a => Opcode::ILoad(LvIndex(0)),
			0x1b => Opcode::ILoad(LvIndex(1)),
			0x1c => Opcode::ILoad(LvIndex(2)),
			0x1d => Opcode::ILoad(LvIndex(3)),
			0xfe => Opcode::ImpDep1,
			0xff => Opcode::ImpDep2,
			0x68 => Opcode::IMul,
			0x74 => Opcode::INeg,
			0xc1 => Opcode::InstanceOf {
				class: pool.get(reader.get_u16_as_usize()?)?,
			},
			0xba => Opcode::InvokeDynamic {
				call_site: pool.get(reader.get_u16_as_usize()?)?,
				zero1: reader.get_u8()?, // == 0
				zero2: reader.get_u8()?, // == 0
			},
			0xb9 => Opcode::InvokeInterface {
				method_ref: pool.get(reader.get_u16_as_usize()?)?,
				count: reader.get_u8()?,
				zero: reader.get_u8()?, // == 0
			},
			0xb7 => Opcode::InvokeSpecial {
				// TODO: JVMS 8, 4.9.1
				method_ref: pool.get(reader.get_u16_as_usize()?)?,
			},
			0xb8 => Opcode::InvokeStatic {
				method_ref: pool.get(reader.get_u16_as_usize()?)?,
			},
			0xb6 => Opcode::InvokeVirtual {
				method_ref: pool.get(reader.get_u16_as_usize()?)?,
			},
			0x80 => Opcode::IOr,
			0x70 => Opcode::IRem,
			0xac => Opcode::IReturn,
			0x78 => Opcode::IShl,
			0x7a => Opcode::IShr,
			0x36 => Opcode::IStore(LvIndex(reader.get_u8_as_usize()?)),
			0x3b => Opcode::IStore(LvIndex(0)),
			0x3c => Opcode::IStore(LvIndex(1)),
			0x3d => Opcode::IStore(LvIndex(2)),
			0x3e => Opcode::IStore(LvIndex(3)),
			0x64 => Opcode::ISub,
			0x7c => Opcode::IUShr,
			0x82 => Opcode::IXor,
			0xa8 => { // jsr
				return Err(ClassFileParseError::IllegalInstruction("jsr instruction is not legal in class files of version 52.0 or greater"));
				//let branch_target = reader.get_i16_branchoffset()?;
				//Opcode::Jsr { branch_target }
			},
			0xc9 => { // jsr_w
				return Err(ClassFileParseError::IllegalInstruction("jsr_w instruction is not legal in class files of version 52.0 or greater"));
				//let branch_target = reader.get_i32_branchoffset()?;
				//Opcode::Jsr { branch_target }
			},
			0x8a => Opcode::L2d,
			0x89 => Opcode::L2f,
			0x88 => Opcode::L2i,
			0x61 => Opcode::LAdd,
			0x2f => Opcode::LALoad,
			0x7f => Opcode::LAnd,
			0x50 => Opcode::LAStore,
			0x94 => Opcode::LCmp,
			0x09 => Opcode::LConst0,
			0x0a => Opcode::LConst1,
			opcode @ (0x12 | 0x13) => {
				let cp_index = match opcode {
					0x12 => reader.get_u8_as_usize()?, // ldc
					0x13 => reader.get_u16_as_usize()?, // ldc_w
					_ => unreachable!(),
				};

				match pool.get(cp_index)? {
					PoolEntry::Integer(int) => {
						Opcode::LdcInt { int }
					},
					PoolEntry::Float(float) => {
						Opcode::LdcFloat { float }
					},
					PoolEntry::String(string) => {
						Opcode::LdcReferenceString { string }
					},
					PoolEntry::Class(class) => {
						Opcode::LdcReferenceClass(class)
					},
					PoolEntry::MethodType(method_type) => {
						Opcode::LdcReferenceMethodType { method_type }
					},
					PoolEntry::MethodHandle(method_handle) => {
						Opcode::LdcReferenceMethodHandle { method_handle }
					},
					_ => todo!("handle error"),
				}
			},
			0x14 => { // ldc2_w
				let cp_index = reader.get_u16_as_usize()?;
				match pool.get(cp_index)? {
					PoolEntry::Long(long) => {
						Opcode::Ldc2WLong { long }
					},
					PoolEntry::Double(double) => {
						Opcode::Ldc2WDouble { double }
					},
					_ => todo!("handle error"),
				}
			},
			0x6d => Opcode::LDiv,
			0x16 => Opcode::LLoad(LvIndex(reader.get_u8_as_usize()?)),
			0x1e => Opcode::LLoad(LvIndex(0)),
			0x1f => Opcode::LLoad(LvIndex(1)),
			0x20 => Opcode::LLoad(LvIndex(2)),
			0x21 => Opcode::LLoad(LvIndex(3)),
			0x69 => Opcode::LMul,
			0x75 => Opcode::LNeg,
			0xab => { // LookupSwitch
				reader.move_to_next_4_byte_boundary();

				let default_target = reader.get_i32_branchoffset()?;
				let npairs = reader.get_i32()?;

				let n = npairs as usize; // TODO: can panic!

				let mut targets = Vec::with_capacity(n);
				for _ in 0..n {
					let match_ = reader.get_i32()?;
					let branch_target = reader.get_i32_branchoffset()?;

					targets.push((match_, branch_target));
				}

				Opcode::LookupSwitch { default_target, npairs, targets }
			},
			0x81 => Opcode::LOr,
			0x71 => Opcode::LRem,
			0xad => Opcode::LReturn,
			0x79 => Opcode::LShl,
			0x7b => Opcode::LShr,
			0x37 => Opcode::LStore(LvIndex(reader.get_u8_as_usize()?)),
			0x3f => Opcode::LStore(LvIndex(0)),
			0x40 => Opcode::LStore(LvIndex(1)),
			0x41 => Opcode::LStore(LvIndex(2)),
			0x42 => Opcode::LStore(LvIndex(3)),
			0x65 => Opcode::LSub,
			0x7d => Opcode::LUShr,
			0x83 => Opcode::LXor,
			0xc2 => Opcode::MonitorEnter,
			0xc3 => Opcode::MonitorExit,
			0xc5 => Opcode::MultiANewArray {
				class: pool.get(reader.get_u16_as_usize()?)?,
				dimensions: reader.get_u8()?, // >= 1
			},
			0xbb => Opcode::New {
				// TODO: may not be array class, 4.9.1
				class: pool.get(reader.get_u16_as_usize()?)?,
			},
			0xbc => Opcode::NewArray {
				a_type: ArrayType::parse(reader.get_u8()?)?,
			},
			0x00 => Opcode::Nop,
			0x57 => Opcode::Pop,
			0x58 => Opcode::Pop2,
			0xb5 => Opcode::PutField(pool.get(reader.get_u16_as_usize()?)?),
			0xb3 => Opcode::PutStatic(pool.get(reader.get_u16_as_usize()?)?),
			0xa9 => {
				return Err(ClassFileParseError::IllegalInstruction("ret instruction is not legal in class files of version 52.0 or greater"));
				//Opcode::Ret {
				//	lv_index: reader.get_u8_as_usize()?,
				//}
			},
			0xb1 => Opcode::Return,
			0x35 => Opcode::SALoad,
			0x56 => Opcode::SAStore,
			0x11 => Opcode::SIPush {
				byte1: reader.get_u8()?,
				byte2: reader.get_u8()?,
			},
			0x5f => Opcode::Swap,
			0xaa => { // TableSwitch
				reader.move_to_next_4_byte_boundary();

				let default_target = reader.get_i32_branchoffset()?;
				let low = reader.get_i32()?;
				let high = reader.get_i32()?;

				let n = (high - low + 1) as usize;

				let mut targets = Vec::with_capacity(n);
				for _ in 0..n {
					let branch_target = reader.get_i32_branchoffset()?;
					targets.push(branch_target);
				}

				Opcode::TableSwitch {
					default_target,
					low,
					high,
					targets,
				}
			},
			0xc4 => { // Wide
				let opcode = reader.get_u8()?;

				match opcode {
					0x19 => Opcode::ALoad(LvIndex(reader.get_u16_as_usize()?)),
					0x3a => Opcode::AStore(LvIndex(reader.get_u16_as_usize()?)),
					0x18 => Opcode::DLoad(LvIndex(reader.get_u16_as_usize()?)),
					0x39 => Opcode::DStore(LvIndex(reader.get_u16_as_usize()?)),
					0x17 => Opcode::FLoad(LvIndex(reader.get_u16_as_usize()?)),
					0x38 => Opcode::FStore(LvIndex(reader.get_u16_as_usize()?)),
					0x15 => Opcode::ILoad(LvIndex(reader.get_u16_as_usize()?)),
					0x36 => Opcode::IStore(LvIndex(reader.get_u16_as_usize()?)),
					0x16 => Opcode::LLoad(LvIndex(reader.get_u16_as_usize()?)),
					0x37 => Opcode::LStore(LvIndex(reader.get_u16_as_usize()?)),
					0xa0 => {
						return Err(ClassFileParseError::IllegalInstruction("wide ret instruction is not legal in class files of version 52.0 or greater"));
						//Opcode::Ret {
						//	lv_index: reader.get_u16_as_usize()?,
						//}
					},
					0x84 => Opcode::IInc {
						lv_index: LvIndex(reader.get_u16_as_usize()?),
						const_: reader.get_i16()? as i32,
					},
					_ => todo!(),
				}
			},
			x => todo!("not implemented: {}", x),
		})
	}

	pub fn parse(bytes: Vec<u8>, pool: &Pool, _: StackMapTableAttribute, exceptions: Vec<ExceptionTableEntry>)
				 -> Result<Code, ClassFileParseError> {


		let mut reader = CodeReader::new(bytes);

		let mut instructions = Vec::new();

		// parse the instructions, using absolute values for jump locations
		while reader.has_elements() {
			reader.next_instruction();
			let opcode = Self::read_next_opcode(&mut reader, pool)?;

			let instruction_pos = reader.this_instruction_pos;

			instructions.push((instruction_pos, opcode));
		}


		//panic!();

		Ok(Code {
			code: Vec::new()
		})
	}
}