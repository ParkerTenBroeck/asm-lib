use crate::expression::conversion::AsmNum;
use crate::simple::trans::TranslationUnitMachine;
use crate::{
    assembler::LangCtx,
    context::{Node, NodeRef},
    expression::{
        AssemblyLabel, AssemblyRegister, CustomValue, ExprCtx, FuncParamParser, Indexed, NodeVal,
        Value, ValueType, binop::BinOp, unop::UnOp,
    },
    lex::Number,
};

use super::*;

pub trait SimpleAssemblyLanguage<'a>: Sized + 'a {
    type Reg: AssemblyRegister<'a, Lang = Self>;
    type Indexed: Indexed<'a, Lang = Self>;
    type CustomValue: CustomValue<'a, Lang = Self>;
    type Label: AssemblyLabel<'a, Lang = Self>;
    type TranslationUnitMachine: TranslationUnitMachine<PtrSizeType = Self::Uptr>;

    type Usize: AsmNum<'a, Self>;
    type Isize: AsmNum<'a, Self>;
    type Uptr: AsmNum<'a, Self>;
    type Iptr: AsmNum<'a, Self>;
    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn parse_ident(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        ident: Node<'a, &'a str>,
        hint: crate::expression::ValueType<'a, Self>,
    ) -> crate::expression::Value<'a, Self>;

    fn parse_numeric_literal(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        num: Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self)
            .parse_numeric_literal_base(num, negated, hint)
    }

    fn eval_func(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).func_base(func, hint)
    }

    fn eval_binop(
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

    fn eval_unnop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        op: Node<'a, UnOp>,
        expr: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).unop_base(node, op, expr, hint)
    }

    #[allow(clippy::too_many_arguments)]
    fn eval_index(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        lhs: Option<NodeVal<'a, Self>>,
        opening: NodeRef<'a>,
        rhs: Option<NodeVal<'a, Self>>,
        closing: NodeRef<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self)
            .index_base(node, lhs, opening, rhs, closing, hint)
    }

    fn eval_cast(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        expr: NodeVal<'a, Self>,
        as_node: NodeRef<'a>,
        ty: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).cast_base(node, expr, as_node, ty, hint)
    }

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeRef<'a>,
    );

    #[allow(unused)]
    fn encounter_label(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        label: &'a str,
        node: NodeRef<'a>,
    ) {
    }

    fn add_value_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        value: Value<'a, Self>,
        n: NodeRef<'a>,
    );
    #[allow(unused)]
    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) {}
    fn state_mut(&mut self) -> &mut SALState<'a, Self>;
    fn state(&self) -> &SALState<'a, Self>;
}
