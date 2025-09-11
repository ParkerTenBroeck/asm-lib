use assembler::{
    Node, NodeRef,
    expression::{ExprCtx, NodeVal, Value, ValueType, binop::BinOp},
};

use crate::{
    MipsAssembler,
    indexed::MemoryIndex,
    label::{self, LabelExpr},
};

impl<'a> MipsAssembler<'a> {
    pub(crate) fn eval_binop_impl(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        lhs: NodeVal<'a, Self>,
        op: Node<'a, BinOp>,
        rhs: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        let lbl_config = ctx.context.config().implicit_cast_label_offset;
        match (op.0, lhs, rhs) {
            (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Indexed(idx), _))
            | (BinOp::Add, Node(Value::Indexed(idx), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                match idx {
                    MemoryIndex::LabelRegisterOffset(register, label) => {
                        Value::Indexed(MemoryIndex::LabelRegisterOffset(
                            register,
                            label.offset(
                                c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                                    .unwrap_or_default(),
                            ),
                        ))
                    }
                    MemoryIndex::RegisterOffset(register, offset) => {
                        Value::Indexed(MemoryIndex::RegisterOffset(
                            register,
                            offset.wrapping_add(
                                c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                                    .unwrap_or_default(),
                            ),
                        ))
                    }
                }
            }

            (BinOp::Sub, Node(Value::Indexed(idx), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                match idx {
                    MemoryIndex::LabelRegisterOffset(register, label) => {
                        Value::Indexed(MemoryIndex::LabelRegisterOffset(
                            register,
                            label.offset(
                                c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                                    .unwrap_or(0i32)
                                    .wrapping_neg(),
                            ),
                        ))
                    }
                    MemoryIndex::RegisterOffset(register, offset) => {
                        Value::Indexed(MemoryIndex::RegisterOffset(
                            register,
                            offset.wrapping_add(
                                c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                                    .unwrap_or(0i32)
                                    .wrapping_neg(),
                            ),
                        ))
                    }
                }
            }

            (BinOp::Add, Node(Value::Register(reg), _), Node(Value::Constant(c), cn))
            | (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Register(reg), _))
                if c.is_integer() =>
            {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    reg,
                    c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                        .unwrap_or(0i32),
                ))
            }

            (BinOp::Sub, Node(Value::Register(reg), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    reg,
                    c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                        .unwrap_or(0i32)
                        .wrapping_neg(),
                ))
            }

            (BinOp::Sub, Node(Value::Label(lhs), _), Node(Value::Label(rhs), _)) => {
                if lhs.pattern.is_some() || rhs.pattern.is_some() {
                    ctx.context.report_error(
                        node,
                        "both lhs and rhs must not have a specified representation",
                    );
                    return Value::Label(LabelExpr::default());
                }
                match (lhs.ty, rhs.ty) {
                    (
                        label::LabelExprType::Unspecified(llhs),
                        label::LabelExprType::Unspecified(lrhs),
                    ) => {
                        return Value::Label(LabelExpr {
                            ty: label::LabelExprType::Sub(llhs, lrhs),
                            offset: lhs.offset.wrapping_add(rhs.offset),
                            pattern: None,
                        });
                    }
                    (label::LabelExprType::Sub(_, _), _) | (_, label::LabelExprType::Sub(_, _)) => {
                        ctx.context
                            .report_error(node, "can only calculate different between two labels");
                    }
                    _ => {
                        ctx.context.report_error(
                            node,
                            "both lhs and rhs must not have a specified relocation kind",
                        );
                    }
                };
                Value::Label(LabelExpr::default())
            }

            (BinOp::Add, Node(Value::Label(l), _), Node(Value::Constant(c), cn))
            | (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Label(l), _))
                if c.is_integer() =>
            {
                Value::Label(
                    l.offset(
                        c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                            .unwrap_or(0i32),
                    ),
                )
            }

            (BinOp::Sub, Node(Value::Label(l), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                Value::Label(
                    l.offset(
                        c.checked_cast_iptr_with(cn, ctx.context, lbl_config)
                            .unwrap_or(0i32)
                            .wrapping_neg(),
                    ),
                )
            }

            _ => ctx.eval(self).binop_base(node, lhs, op, rhs, hint),
        }
    }
}
