use std::fmt::{Display, Formatter};

use crate::{MipsAssembler, label::LabelExpr, reg::Register};
use assembler::expression::Indexed;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MemoryIndex<'a> {
    LabelRegisterOffset(Register, LabelExpr<'a>),
    RegisterOffset(Register, i32),
}

impl<'a> Indexed<'a> for MemoryIndex<'a> {
    type Lang = MipsAssembler<'a>;
}

impl<'a> Display for MemoryIndex<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            MemoryIndex::LabelRegisterOffset(reg, l) => {
                if l.needs_parens() {
                    write!(f, "(")?;
                }
                write!(f, "{l}")?;
                if l.needs_parens() {
                    write!(f, ")")?;
                }
                if reg.0 != 0 {
                    write!(f, "[{reg}]")?;
                }
                Ok(())
            }
            MemoryIndex::RegisterOffset(reg, offset) => {
                if reg.0 == 0 {
                    write!(f, "{offset}")?;
                } else {
                    write!(f, "{reg}")?;
                    if offset != 0 {
                        write!(f, "[{offset}]")?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl<'a> Default for MemoryIndex<'a> {
    fn default() -> Self {
        Self::RegisterOffset(Register::default(), 0)
    }
}
