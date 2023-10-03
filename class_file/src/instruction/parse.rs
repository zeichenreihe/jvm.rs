use anyhow::Result;
use std::fmt::{Debug, Formatter};
use std::mem::size_of;
use crate::cp::attribute::{ExceptionTableEntry, StackMapTableAttribute};
use crate::cp::Pool;
use crate::instruction::opcode::Opcode;

pub struct OutOfBoundsError;

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
	fn read_next_opcode(reader: &mut CodeReader, pool: &Pool) -> Result<Opcode> {
		todo!()
	}

	pub fn parse(bytes: Vec<u8>, pool: &Pool, _: StackMapTableAttribute, exceptions: Vec<ExceptionTableEntry>)
				 -> Result<Code> {


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