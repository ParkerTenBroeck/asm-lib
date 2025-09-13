use std::fmt::{Display, Formatter};

use assembler::{Assembler, expression::AssemblyLabel, simple::SimpleAssemblyLanguage};

use crate::{MipsAssembler, trans::MipsRelocCalc};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RelocPattern {
    High,
    Low,
    U8,
    U16,
    U32,
}

impl std::fmt::Display for RelocPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RelocPattern::High => write!(f, "hi"),
            RelocPattern::Low => write!(f, "lo"),
            RelocPattern::U8 => write!(f, "u8"),
            RelocPattern::U16 => write!(f, "u16"),
            RelocPattern::U32 => write!(f, "u32"),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct Label<'a> {
    pub ident: &'a str,
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub enum LabelExprType<'a> {
    #[default]
    Empty,
    Unspecified(Label<'a>),
    PcRel(Label<'a>),
    Absolute(Label<'a>),
    Size(Label<'a>),
    Align(Label<'a>),
    Sub(Label<'a>, Label<'a>),
}
impl<'a> LabelExprType<'a> {
    pub fn reloc_type(
        &self,
        ctx: &mut Assembler<'a, '_, MipsAssembler<'a>>,
        pcrel: bool,
    ) -> Option<MipsRelocCalc> {
        let trans = &mut ctx.lang.state_mut().trans;
        Some(match self {
            LabelExprType::Empty => None?,
            LabelExprType::Unspecified(label) if pcrel => {
                MipsRelocCalc::Pcrel(trans.resolve_or_make_symbol(label.ident))
            }
            LabelExprType::Unspecified(label) => {
                MipsRelocCalc::Absolute(trans.resolve_or_make_symbol(label.ident))
            }
            LabelExprType::PcRel(label) => {
                MipsRelocCalc::Absolute(trans.resolve_or_make_symbol(label.ident))
            }
            LabelExprType::Absolute(label) => {
                MipsRelocCalc::Pcrel(trans.resolve_or_make_symbol(label.ident))
            }
            LabelExprType::Size(label) => {
                MipsRelocCalc::Size(trans.resolve_or_make_symbol(label.ident))
            }
            LabelExprType::Align(label) => {
                MipsRelocCalc::Align(trans.resolve_or_make_symbol(label.ident))
            }
            LabelExprType::Sub(lhs, rhs) => MipsRelocCalc::Sub(
                trans.resolve_or_make_symbol(lhs.ident),
                trans.resolve_or_make_symbol(rhs.ident),
            ),
        })
    }
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct LabelExpr<'a> {
    pub ty: LabelExprType<'a>,
    pub offset: i32,
    pub pattern: Option<RelocPattern>,
}
impl<'a> LabelExpr<'a> {
    pub fn needs_parens(&self) -> bool {
        match self.ty {
            LabelExprType::Sub(_, _) => self.pattern.is_none(),
            LabelExprType::Unspecified(_) => self.offset != 0,
            _ => false,
        }
    }

    pub fn offset(mut self, offset: i32) -> Self {
        self.offset = self.offset.wrapping_add(offset);
        self
    }

    pub fn new(ident: &'a str) -> Self {
        Self {
            ty: LabelExprType::Unspecified(Label::new(ident)),
            offset: 0,
            pattern: None,
        }
    }
}

impl<'a> Label<'a> {
    pub fn new(ident: &'a str) -> Self {
        Label { ident }
    }
}

impl<'a> Display for LabelExpr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(pattern) = self.pattern {
            write!(f, "{pattern}(")?;
        }
        match self.ty {
            LabelExprType::Empty => write!(f, "<EMPTY>")?,
            LabelExprType::Unspecified(label) => write!(f, "{label}")?,
            LabelExprType::PcRel(label) => write!(f, "pcrel({label})")?,
            LabelExprType::Absolute(label) => write!(f, "absolute({label})")?,
            LabelExprType::Size(label) => write!(f, "size({label})")?,
            LabelExprType::Align(label) => write!(f, "align({label})")?,
            LabelExprType::Sub(lhs, rhs) => write!(f, "{lhs}-{rhs}")?,
        }
        if self.offset != 0 {
            write!(f, "{:+}", self.offset)?;
        }
        if self.pattern.is_some() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl<'a> AssemblyLabel<'a> for LabelExpr<'a> {
    type Lang = MipsAssembler<'a>;
}
