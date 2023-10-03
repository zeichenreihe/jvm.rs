use crate::instruction::LvIndex;
use crate::instruction::opcode::Opcode;

#[allow(unused)]
enum LegacyOpcode {
	Modern(Opcode),

	Jsr { branch_target: usize },
	Ret(LvIndex),
}