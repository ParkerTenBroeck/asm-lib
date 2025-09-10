use assembler::{
    NodeRef,
    expression::{ExprCtx, NodeVal, Value, ValueType},
};

use crate::{MipsAssembler, indexed::MemoryIndex};

impl<'a> MipsAssembler<'a> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn eval_index_impl(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        lhs: Option<NodeVal<'a, Self>>,
        opening: NodeRef<'a>,
        rhs: Option<NodeVal<'a, Self>>,
        closing: NodeRef<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
            return ctx
                .eval(self)
                .index_base(node, lhs, opening, rhs, closing, hint);
        };
        match (lhs.0, rhs.0) {
            (Value::Register(r), Value::Constant(c)) | (Value::Constant(c), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    r,
                    c.checked_cast_iptr(node, ctx.context).unwrap_or(0),
                ))
            }
            (Value::Indexed(MemoryIndex::RegisterOffset(r, o)), Value::Constant(c))
            | (Value::Constant(c), Value::Indexed(MemoryIndex::RegisterOffset(r, o))) => {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    r,
                    o.wrapping_add(c.checked_cast_iptr(node, ctx.context).unwrap_or(0)),
                ))
            }

            (Value::Label(l), Value::Constant(c)) | (Value::Constant(c), Value::Label(l)) => {
                Value::Label(l.offset(c.checked_cast_iptr(node, ctx.context).unwrap_or(0)))
            }
            (Value::Label(l), Value::Register(r)) | (Value::Register(r), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }

            (Value::Label(l), Value::Indexed(MemoryIndex::RegisterOffset(r, i)))
            | (Value::Indexed(MemoryIndex::RegisterOffset(r, i)), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l.offset(i)))
            }
            _ => ctx
                .eval(self)
                .index_base(node, Some(lhs), opening, Some(rhs), closing, hint),
        }
    }
}
