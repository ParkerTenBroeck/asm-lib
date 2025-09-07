pub mod indexed;
pub mod label;
pub mod lang;
pub mod opcodes;
pub mod reg;
pub mod trans;

pub use lang::MipsAssembler;
pub type NodeVal<'a> = assembler::expression::NodeVal<'a, MipsAssembler<'a>>;

pub fn main() {}
