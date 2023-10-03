use anyhow::{anyhow, Result};
use crate::cp::Pool;
use crate::instruction::opcode::Opcode;
use crate::MyRead;

/// Contains the pre-verifier opcodes.
mod old;

pub mod opcode;

/// Describes a local variable index. Is used in [Opcode::ALoad] and others.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LvIndex(pub usize);

/// Describes a target of a branch
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BranchTarget(pub usize);


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instructions {
	inner: Vec<Instruction>,
}

impl Instructions {
	pub(crate) fn parse(bytes: &[u8], pool: &Pool) -> Result<Instructions> {
		let mut instructions = Vec::new();

		let mut reader = OpcodeReader::new(bytes);
		while reader.pos < bytes.len() {
			reader.next_instruction();
			let instruction = Instruction::parse(&mut reader, pool)?;

			instructions.push(instruction);
		}

		Ok(Instructions { inner: instructions })
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
	offset: usize,
	opcode: Opcode,
}

impl Instruction {
	fn parse<R: MyRead>(reader: &mut OpcodeReader<R>, pool: &Pool) -> Result<Instruction> {
		Ok(Instruction {
			offset: reader.pos,
			opcode: Opcode::parse(reader, pool)?,
		})
	}
}

struct OpcodeReader<R: std::io::Read> {
	reader: R,
	pos: usize,
	current_instruction_pos: usize,
}

impl<R: std::io::Read> OpcodeReader<R> {
	fn new(reader: R) -> OpcodeReader<R> {
		OpcodeReader {
			reader,
			pos: 0,
			current_instruction_pos: 0,
		}
	}
}

impl<R: std::io::Read> std::io::Read for OpcodeReader<R> {
	fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
		let n = self.reader.read(buf)?;
		self.pos += n;
		Ok(n)
	}
}

pub(crate) trait CodeReader: MyRead {
	fn next_instruction(&mut self);
	fn read_i16_branchoffset(&mut self) -> Result<BranchTarget>;
	fn read_i32_branchoffset(&mut self) -> Result<BranchTarget>;
	fn move_to_next_4_byte_boundary(&mut self) -> Result<()>;
}

impl<R: std::io::Read> CodeReader for OpcodeReader<R> {
	fn next_instruction(&mut self) {
		self.current_instruction_pos = self.pos
	}

	fn read_i16_branchoffset(&mut self) -> Result<BranchTarget> {
		let offset: isize = self.read_i16()?.try_into()?;
		let this_pos: isize = self.current_instruction_pos.try_into()?;

		let target: usize = offset.checked_add(this_pos)
			.ok_or_else(|| anyhow!("overflow in reading i16 branchoffset: instruction pos: {this_pos}, read offset: {offset}"))?
			.try_into()?;

		Ok(BranchTarget(target))
	}

	fn read_i32_branchoffset(&mut self) -> Result<BranchTarget> {
		let offset = self.read_i32()?;
		let target = offset + (self.current_instruction_pos as i32);

		Ok(BranchTarget(target.try_into().unwrap()))//TODO: panic!
	}

	fn move_to_next_4_byte_boundary(&mut self) -> Result<()> {
		match self.pos % 4 {
			0 => {},
			1 => drop(self.read_n::<3>()?),
			2 => drop(self.read_n::<2>()?),
			3 => drop(self.read_n::<1>()?),
			_ => unreachable!("usize % 4 can only give 0..4"),
		}
		Ok(())
	}
}