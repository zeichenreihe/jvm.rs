mod type_checking;
mod type_inference;

pub use type_checking::*;
pub use type_inference::*;

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