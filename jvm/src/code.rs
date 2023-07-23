use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::mem::size_of;
use crate::classfile::ConstantPool;
use crate::errors::{ClassFileParseError, OutOfBoundsError};
use crate::opcodes::{ArrayType, Opcode};

struct CodeReader {
	bytes: Vec<u8>,
	pos: usize,
	instruction_count: usize,
	this_instruction_pos: usize,
	pc_map: PcMap,
}

impl CodeReader {
	fn has_elements(&self) -> bool {
		self.pos < self.bytes.len()
	}

	fn next_instruction(&mut self) -> () {
		self.pc_map.0.insert(self.pos, self.instruction_count);
		self.instruction_count += 1;
		self.this_instruction_pos = self.pos;
	}

	fn remap_pc(&self, target: &mut usize) -> () {
		self.pc_map.remap_pc(target);
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

#[derive(Debug, Clone, PartialEq, Eq)]
/// Maps from the "old" program counter, that is, the one in the original bytecode, to "our" program counter, that is, the index in the code vec.
pub struct PcMap(HashMap<usize, usize>);

impl PcMap {
	fn remap_pc(&self, target: &mut usize) -> () {
		*target = *self.0.get(&target).expect("no such instruction at bytecode ...");
	}

	pub fn map(&self, target: usize) -> Result<usize, ClassFileParseError> {
		Ok(*self.0.get(&target).expect("no such instruction at bytecode ..."))
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct Code {
	pub code: Vec<Opcode>,
	pub pc_map: PcMap,
}

impl Debug for Code {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let mut t = f.debug_struct("Code");
		t.field("pc_map", &self.pc_map);
		for (pos, entry) in self.code.iter().enumerate() {
			t.field(&format!("- {pos:?}"), &entry);
		}
		t.finish()
	}
}

impl Code {
	pub fn parse(bytes: Vec<u8>, constant_pool: &ConstantPool) -> Result<Code, ClassFileParseError> {
		let mut bytes = CodeReader {
			bytes,
			pos: 0,
			instruction_count: 0,
			this_instruction_pos: 0,
			pc_map: PcMap(HashMap::new()),
		};

		let mut code = Vec::new();

		// parse the instructions, using absolute values for jump locations
		while bytes.has_elements() {
			bytes.next_instruction();
			let opcode = match bytes.get_u8()? {
				0x32 => Opcode::AALoad,
				0x53 => Opcode::AAStore,
				0x01 => Opcode::AConstNull,
				0x19 => Opcode::ALoad {
					lv_index: bytes.get_u8()?,
				},
				0x2a => Opcode::ALoad0,
				0x2b => Opcode::ALoad1,
				0x2c => Opcode::ALoad2,
				0x2d => Opcode::ALoad3,
				0xbd => Opcode::ANewArray {
					class: constant_pool.get(bytes.get_u16_as_usize()?)?
				},
				0xb0 => Opcode::AReturn,
				0xbe => Opcode::ArrayLength,
				0x3a => Opcode::AStore {
					lv_index: bytes.get_u8()?,
				},
				0x4b => Opcode::AStore0,
				0x4c => Opcode::AStore1,
				0x4d => Opcode::AStore2,
				0x4e => Opcode::AStore3,
				0xbf => Opcode::AThrow,
				0x33 => Opcode::BALoad,
				0x54 => Opcode::BAStore,
				0x10 => Opcode::BIPush {
					byte: bytes.get_u8()?,
				},
				0xca => Opcode::Breakpoint,
				0x34 => Opcode::CALoad,
				0x55 => Opcode::CAStore,
				0xc0 => Opcode::CheckCast {
					class: constant_pool.get(bytes.get_u16_as_usize()?)?,
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
				0x18 => Opcode::DLoad {
					lv_index: bytes.get_u8()?,
				},
				0x26 => Opcode::DLoad0,
				0x27 => Opcode::DLoad1,
				0x28 => Opcode::DLoad2,
				0x29 => Opcode::DLoad3,
				0x6b => Opcode::DMul,
				0x77 => Opcode::DNeg,
				0x73 => Opcode::DRem,
				0xaf => Opcode::DReturn,
				0x39 => Opcode::DStore {
					lv_index: bytes.get_u8()?,
				},
				0x47 => Opcode::DStore0,
				0x48 => Opcode::DStore1,
				0x49 => Opcode::DStore2,
				0x4a => Opcode::DStore3,
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
				0x17 => Opcode::FLoad {
					lv_index: bytes.get_u8()?,
				},
				0x22 => Opcode::FLoad0,
				0x23 => Opcode::FLoad1,
				0x24 => Opcode::FLoad2,
				0x25 => Opcode::FLoad3,
				0x6a => Opcode::FMul,
				0x76 => Opcode::FNeg,
				0x72 => Opcode::FRem,
				0xae => Opcode::FReturn,
				0x38 => Opcode::FStore {
					lv_index: bytes.get_u8()?,
				},
				0x43 => Opcode::FStore0,
				0x44 => Opcode::FStore1,
				0x45 => Opcode::FStore2,
				0x46 => Opcode::FStore3,
				0x66 => Opcode::FSub,
				0xb4 => Opcode::GetField {
					field_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xb2 => Opcode::GetStatic {
					field_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xa7 => Opcode::Goto {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xc8 => Opcode::GotoW {
					branch_target: bytes.get_i32_branchoffset()?,
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
				0xa5 => Opcode::IfACmpEq {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xa6 => Opcode::IfACmpNe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x9f => Opcode::IfICmpEq {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xa2 => Opcode::IfICmpGe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xa3 => Opcode::IfICmpGt {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xa4 => Opcode::IfICmpLe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xa1 => Opcode::IfICmpLt {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xa0 => Opcode::IfICmpNe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x99 => Opcode::IfEq {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x9c => Opcode::IfGe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x9d => Opcode::IfGt {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x9e => Opcode::IfLe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x9b => Opcode::IfLt {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x9a => Opcode::IfNe {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xc7 => Opcode::IfNonNull {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xc6 => Opcode::IfNull {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0x84 => Opcode::IInc {
					lv_index: bytes.get_u8()?,
					const_: bytes.get_u8()?,
				},
				0x15 => Opcode::ILoad {
					lv_index: bytes.get_u8()?,
				},
				0x1a => Opcode::ILoad0,
				0x1b => Opcode::ILoad1,
				0x1c => Opcode::ILoad2,
				0x1d => Opcode::ILoad3,
				0xfe => Opcode::ImpDep1,
				0xff => Opcode::ImpDep2,
				0x68 => Opcode::IMul,
				0x74 => Opcode::INeg,
				0xc1 => Opcode::InstanceOf {
					class: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xba => Opcode::InvokeDynamic {
					call_site: constant_pool.get(bytes.get_u16_as_usize()?)?,
					zero1: bytes.get_u8()?, // == 0
					zero2: bytes.get_u8()?, // == 0
				},
				0xb9 => Opcode::InvokeInterface {
					method_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
					count: bytes.get_u8()?,
					zero: bytes.get_u8()?, // == 0
				},
				0xb7 => Opcode::InvokeSpecial {
					method_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xb8 => Opcode::InvokeStatic {
					method_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xb6 => Opcode::InvokeVirtual {
					method_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0x80 => Opcode::IOr,
				0x70 => Opcode::IRem,
				0xac => Opcode::IReturn,
				0x78 => Opcode::IShl,
				0x7a => Opcode::IShr,
				0x36 => Opcode::IStore {
					lv_index: bytes.get_u8()?,
				},
				0x3b => Opcode::IStore0,
				0x3c => Opcode::IStore1,
				0x3d => Opcode::IStore2,
				0x3e => Opcode::IStore3,
				0x64 => Opcode::ISub,
				0x7c => Opcode::IUShr,
				0x82 => Opcode::IXor,
				0xa8 => Opcode::Jsr {
					branch_target: bytes.get_i16_branchoffset()?,
				},
				0xc9 => Opcode::JsrW {
					branch_target: bytes.get_i32_branchoffset()?,
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
				0x12 => Opcode::Ldc { // TODO: make this store the *Info structure here
					cp_index: bytes.get_u8()? as u16,
				},
				0x13 => Opcode::LdcW { // TODO: make this store the *Info structure here
					cp_index: bytes.get_u16()?,
				},
				0x14 => Opcode::Ldc2W { // TODO: make this store the *Info structure here
					cp_index: bytes.get_u16()?,
				},
				0x6d => Opcode::LDiv,
				0x16 => Opcode::LLoad {
					lv_index: bytes.get_u8()?,
				},
				0x1e => Opcode::LLoad0,
				0x1f => Opcode::LLoad1,
				0x20 => Opcode::LLoad2,
				0x21 => Opcode::LLoad3,
				0x69 => Opcode::LMul,
				0x75 => Opcode::LNeg,
				0xab => { // LookupSwitch
					bytes.move_to_next_4_byte_boundary();

					let default_target = bytes.get_i32_branchoffset()?;
					let npairs = bytes.get_i32()?;

					let n = npairs as usize; // TODO: can panic!

					let mut targets = Vec::with_capacity(n);
					for _ in 0..n {
						let match_ = bytes.get_i32()?;
						let target = bytes.get_i32_branchoffset()?;

						targets.push((match_, target));
					}

					Opcode::LookupSwitch { default_target, npairs, targets }
				},
				0x81 => Opcode::LOr,
				0x71 => Opcode::LRem,
				0xad => Opcode::LReturn,
				0x79 => Opcode::LShl,
				0x7b => Opcode::LShr,
				0x37 => Opcode::LStore {
					lv_index: bytes.get_u8()?,
				},
				0x3f => Opcode::LStore0,
				0x40 => Opcode::LStore1,
				0x41 => Opcode::LStore2,
				0x42 => Opcode::LStore3,
				0x65 => Opcode::LSub,
				0x7d => Opcode::LUShr,
				0x83 => Opcode::LXor,
				0xc2 => Opcode::MonitorEnter,
				0xc3 => Opcode::MonitorExit,
				0xc5 => Opcode::MultiANewArray {
					class: constant_pool.get(bytes.get_u16_as_usize()?)?,
					dimensions: bytes.get_u8()?, // >= 1
				},
				0xbb => Opcode::New {
					class: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xbc => Opcode::NewArray {
					a_type: ArrayType::parse(bytes.get_u8()?)?,
				},
				0x00 => Opcode::Nop,
				0x57 => Opcode::Pop,
				0x58 => Opcode::Pop2,
				0xb5 => Opcode::PutField {
					field_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xb3 => Opcode::PutStatic {
					field_ref: constant_pool.get(bytes.get_u16_as_usize()?)?,
				},
				0xa9 => Opcode::Ret {
					lv_index: bytes.get_u8()?,
				},
				0xb1 => Opcode::Return,
				0x35 => Opcode::SALoad,
				0x56 => Opcode::SAStore,
				0x11 => Opcode::SIPush {
					byte1: bytes.get_u8()?,
					byte2: bytes.get_u8()?,
				},
				0x5f => Opcode::Swap,
				0xaa => { // TableSwitch
					bytes.move_to_next_4_byte_boundary();

					let default_target = bytes.get_i32_branchoffset()?;
					let low = bytes.get_i32()?;
					let high = bytes.get_i32()?;

					let n = (high - low + 1) as usize;

					let mut targets = Vec::with_capacity(n);
					for _ in 0..n {
						targets.push(bytes.get_i32_branchoffset()?);
					}

					Opcode::TableSwitch {
						default_target, low, high, targets,
					}
				},
				0xc4 => { // Wide
					let opcode = bytes.get_u8()?;

					match opcode {
						0x19 => Opcode::WideALoad {
							lv_index: bytes.get_u16()?,
						},
						0x3a => Opcode::WideAStore {
							lv_index: bytes.get_u16()?,
						},
						0x18 => Opcode::WideDLoad {
							lv_index: bytes.get_u16()?,
						},
						0x39 => Opcode::WideDStore {
							lv_index: bytes.get_u16()?,
						},
						0x17 => Opcode::WideFLoad {
							lv_index: bytes.get_u16()?,
						},
						0x38 => Opcode::WideFStore {
							lv_index: bytes.get_u16()?,
						},
						0x15 => Opcode::WideILoad {
							lv_index: bytes.get_u16()?,
						},
						0x36 => Opcode::WideIStore {
							lv_index: bytes.get_u16()?,
						},
						0x16 => Opcode::WideLLoad {
							lv_index: bytes.get_u16()?,
						},
						0x37 => Opcode::WideLStore {
							lv_index: bytes.get_u16()?,
						},
						0xa0 => Opcode::WideRet {
							lv_index: bytes.get_u16()?,
						},
						0x84 => Opcode::WideIInc {
							lv_index: bytes.get_u16()?,
							const_: bytes.get_i16()?,
						},
						_ => todo!(),
					}
				},
				x => todo!("not implemented: {}", x),
			};

			code.push(opcode);
		}

		// convert the instruction addresses from "old" to "new"
		code.iter_mut()
			.for_each(|opcode| {
				match opcode {
					Opcode::Goto      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::GotoW     { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfACmpEq  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfACmpNe  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfICmpEq  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfICmpGe  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfICmpGt  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfICmpLe  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfICmpLt  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfICmpNe  { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfEq      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfGe      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfGt      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfLe      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfLt      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfNe      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfNonNull { branch_target } => bytes.remap_pc(branch_target),
					Opcode::IfNull    { branch_target } => bytes.remap_pc(branch_target),
					Opcode::Jsr       { branch_target } => bytes.remap_pc(branch_target),
					Opcode::JsrW      { branch_target } => bytes.remap_pc(branch_target),
					Opcode::LookupSwitch { default_target, targets, .. } => {
						bytes.remap_pc(default_target);
						targets.iter_mut().for_each(|(_, target)| bytes.remap_pc(target));
					},
					Opcode::TableSwitch { default_target, targets, .. } => {
						bytes.remap_pc(default_target);
						targets.iter_mut().for_each(|target| bytes.remap_pc(target));
					},
					_ => {},
				}
			});

		Ok(Code {
			code, pc_map: bytes.pc_map,
		})
	}
}