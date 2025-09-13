use assembler::{
    Context, NodeRef,
    expression::{Constant, Value, ValueType, args::CoercedArg},
};

use crate::{MipsAssembler, indexed::MemoryIndex, label::LabelExpr, reg::Register};

#[derive(Clone, Copy)]
pub enum Immediate<'a> {
    SignedConstant(i32),
    UnsignedConstant(u32),
    Label(LabelExpr<'a>),
}
impl<'a> Default for Immediate<'a> {
    fn default() -> Self {
        Self::UnsignedConstant(0)
    }
}
impl<'a> CoercedArg<'a> for Immediate<'a> {
    type LANG = MipsAssembler<'a>;
    const TYPE_REPR: &'static str = "isize|usize|label";
    const HINT: ValueType<'a, MipsAssembler<'a>> = ValueType::Isize;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        value: Value<'a, MipsAssembler<'a>>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => match c {
                c if c.is_signed_integer() => Ok(Immediate::SignedConstant(
                    c.checked_cast_isize(node, context).ok_or(None)?,
                )),
                c if c.is_unsigned_integer() => Ok(Immediate::UnsignedConstant(
                    c.checked_cast_usize(node, context).ok_or(None)?,
                )),
                _ => Ok(Immediate::SignedConstant(
                    c.checked_cast_isize(node, context).ok_or(None)?,
                )),
            },
            Value::Label(label) => Ok(Immediate::Label(label)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Default::default()
    }
}

pub enum ImmediateU16<'a> {
    Constant(u16),
    Label(LabelExpr<'a>),
}
impl<'a> Default for ImmediateU16<'a> {
    fn default() -> Self {
        Self::Constant(0)
    }
}
impl<'a> CoercedArg<'a> for ImmediateU16<'a> {
    type LANG = MipsAssembler<'a>;
    const TYPE_REPR: &'static str = "u16|label";
    const HINT: ValueType<'a, MipsAssembler<'a>> = ValueType::U16;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, MipsAssembler<'a>>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::U16(v)) => Ok(Self::Constant(v)),
            Value::Label(label) => Ok(Self::Label(label)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Default::default()
    }
}

pub enum ImmediateI16<'a> {
    Constant(i16),
    Label(LabelExpr<'a>),
}
impl<'a> Default for ImmediateI16<'a> {
    fn default() -> Self {
        Self::Constant(0)
    }
}
impl<'a> CoercedArg<'a> for ImmediateI16<'a> {
    type LANG = MipsAssembler<'a>;
    const TYPE_REPR: &'static str = "i16|label";
    const HINT: ValueType<'a, MipsAssembler<'a>> = ValueType::I16;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, MipsAssembler<'a>>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::I16(v)) => Ok(Self::Constant(v)),
            Value::Label(label) => Ok(Self::Label(label)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Default::default()
    }
}

pub struct ShiftConstant(pub u32);

impl<'a> CoercedArg<'a> for ShiftConstant {
    type LANG = MipsAssembler<'a>;
    const TYPE_REPR: &'static str = "integer";
    const HINT: ValueType<'a, MipsAssembler<'a>> = ValueType::I32;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) if c.is_integer() => Ok(Self(
                c.checked_cast_u32_with(node, context, context.config().implicit_cast_shift_value)
                    .unwrap_or_default(),
            )),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self(0)
    }
}

pub struct IndexedCoerced<'a>(pub MemoryIndex<'a>);

impl<'a> CoercedArg<'a> for IndexedCoerced<'a> {
    type LANG = MipsAssembler<'a>;
    const TYPE_REPR: &'static str = "indexed|register|label|iptr|uptr";
    const HINT: ValueType<'a, Self::LANG> = ValueType::Iptr;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => Ok(Self(MemoryIndex::RegisterOffset(
                Register::ZERO,
                c.checked_cast_iptr_with(
                    node,
                    context,
                    context.config().implicit_cast_label_offset,
                )
                .unwrap_or_default(),
            ))),
            Value::Label(l) => Ok(Self(MemoryIndex::LabelRegisterOffset(Register::ZERO, l))),
            Value::Indexed(i) => Ok(Self(i)),
            Value::Register(r) => Ok(Self(MemoryIndex::RegisterOffset(r, 0))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self(Default::default())
    }
}
