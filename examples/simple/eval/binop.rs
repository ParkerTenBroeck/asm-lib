use assembler::{
    Node, NodeRef,
    expression::{ExprCtx, NodeVal, Value, ValueType, binop::BinOp},
};

use crate::MipsAssembler;

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
        ctx.eval(self).binop_base(node, lhs, op, rhs, hint)
    }
}
