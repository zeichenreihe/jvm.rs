use anyhow::Result;
use crate::cp::Pool;
use crate::instruction::opcode::Opcode;
use crate::MyRead;

/// Contains the pre-verifier opcodes.
mod old;

mod parse;

pub mod opcode;

/// Describes a local variable index. Is used in [Opcode::ALoad] and others.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LvIndex(pub usize);

/// Describes a target of a branch
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BranchTarget(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnconditionalBranch(pub BranchTarget);
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionalBranch(pub BranchTarget);


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instructions {
	inner: Vec<Instruction>,
}

impl Instructions {
	pub(crate) fn parse(bytes: &[u8], pool: &Pool) -> Result<Instructions> {
		let mut instructions = Vec::new();

		let mut pos = 0;
		let mut instruction_count = 0;
		let mut this_instruction_pos = 0;

		let mut reader = bytes;
		while pos < bytes.len() {
			instruction_count += 1;
			this_instruction_pos = pos;

			let instruction = Instruction::parse(pos, &mut reader, pool)?;

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
	fn parse<R: MyRead>(offset: usize, reader: &mut R, pool: &Pool) -> Result<Instruction> {
		Ok(Instruction {
			offset,
			opcode: Opcode::parse(reader, pool)?,
		})
	}
}