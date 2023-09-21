use crate::classfile::instruction::{LvIndex, Opcode};

enum LegacyOpcode {
	Modern(Opcode),


	Jsr { branch_target: usize },
	Ret(LvIndex),
}