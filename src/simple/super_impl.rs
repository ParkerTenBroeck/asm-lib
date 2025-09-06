use crate::expression::args::{AsmStrArg, UptrArg, UptrPow2Arg};
use crate::node::NodeTrait;
use crate::simple::trans::TranslationUnit;
use crate::{
    assembler::LangCtx,
    context::{Node, NodeRef},
    expression::{
        ArgumentsTypeHint, AsmStr, Constant, ExprCtx, FuncParamParser, NodeVal, Value, ValueType,
        args::StrArg, binop::BinOp, unop::UnOp,
    },
    lex::Number,
    util::IntoStrDelimable,
};

use num_traits::AsPrimitive;
use std::os::unix::ffi::OsStrExt;

use super::*;

impl<'a, T: SimpleAssemblyLanguage<'a>> crate::assembler::lang::AssemblyLanguage<'a> for T {
    type Reg = T::Reg;
    type Indexed = T::Indexed;
    type CustomValue = T::CustomValue;
    type Label = T::Label;
    type AssembledResult =
        TranslationUnit<<Self as SimpleAssemblyLanguage<'a>>::TranslationUnitMachine>;

    type Usize = T::Usize;
    type Isize = T::Isize;
    type Uptr = T::Uptr;
    type Iptr = T::Iptr;
    type Ufunc = T::Ufunc;
    type Ifunc = T::Ifunc;

    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn parse_ident(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        ident: Node<'a, &'a str>,
        hint: crate::expression::ValueType<'a, Self>,
    ) -> crate::expression::Value<'a, Self> {
        match ident.0 {
            "__line__" => Value::Constant(Constant::U32(ident.1.top().span.line.wrapping_add(1))),
            "__col__" => Value::Constant(Constant::U32(ident.1.top().span.col)),
            "__len__" => Value::Constant(Constant::U32(ident.1.top().span.len)),
            "__offset__" => Value::Constant(Constant::U32(ident.1.top().span.offset)),
            "__file__" => Value::Constant(Constant::Str(
                if let Some(str) = ident.1.top().source.path.as_os_str().to_str() {
                    AsmStr::Str(str)
                } else {
                    AsmStr::ByteStr(ident.1.top().source.path.as_os_str().as_bytes())
                },
            )),
            _ => self.parse_ident(ctx, ident, hint),
        }
    }

    fn parse_numeric_literal(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        num: Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.parse_numeric_literal(ctx, num, negated, hint)
    }

    fn eval_func(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.eval_func(ctx, func, hint)
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
        self.eval_binop(ctx, node, lhs, op, rhs, hint)
    }

    fn eval_unnop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        op: Node<'a, UnOp>,
        expr: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.eval_unnop(ctx, node, op, expr, hint)
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
        self.eval_index(ctx, node, lhs, opening, rhs, closing, hint)
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
        self.eval_cast(ctx, node, expr, as_node, ty, hint)
    }

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeRef<'a>,
    ) {
        macro_rules! constant {
            ($argument:ident, $kind:ident) => {
                for Node(crate::expression::args::$argument::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(ctx, Constant::$kind(arg.unwrap_or_default()), n);
                }
            };
        }
        match mnemonic {
            ".dbg" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context.report_info(args_node, format!("{args:#?}"))
            }
            ".info" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context
                    .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".warning" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context
                    .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".error" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context
                    .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
            }

            ".section" => {
                if let StrArg::Val(Some(sec)) = ctx.eval(self).coerced(n).0 {
                    self.set_section(ctx, sec, n);
                }
            }
            ".align" => {
                if let UptrPow2Arg::Val(Some(align)) = ctx.eval(self).coerced(n).0 {
                    todo!("align: {align}")
                }
            }

            ".label" => {
                if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {
                    self.encounter_label(ctx, label, n);
                }
            }
            ".global" => {
                if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {
                    let section = self.state_mut().expect_section(ctx.context, n);
                    let node = ctx.context.node_to_owned(n);
                    let result = self
                        .state_mut()
                        .trans
                        .resolve_mut(section)
                        .set_symbol_visibility(
                            label,
                            trans::sym::SymbolVisibility::Global,
                            Some(node.clone()),
                        );
                    if let Err(err) = result {
                        ctx.context.report_owned(err.to_log_entry(label, node));
                    }
                }
            }
            ".weak" => {
                if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {
                    let section = self.state_mut().expect_section(ctx.context, n);
                    let node = ctx.context.node_to_owned(n);
                    let result = self
                        .state_mut()
                        .trans
                        .resolve_mut(section)
                        .set_symbol_visibility(
                            label,
                            trans::sym::SymbolVisibility::Weak,
                            Some(node.clone()),
                        );
                    if let Err(err) = result {
                        ctx.context.report_owned(err.to_log_entry(label, node));
                    }
                }
            }
            ".local" => {
                if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {
                    let section = self.state_mut().expect_section(ctx.context, n);
                    let node = ctx.context.node_to_owned(n);
                    let result = self
                        .state_mut()
                        .trans
                        .resolve_mut(section)
                        .set_symbol_visibility(
                            label,
                            trans::sym::SymbolVisibility::Local,
                            Some(node.clone()),
                        );
                    if let Err(err) = result {
                        ctx.context.report_owned(err.to_log_entry(label, node));
                    }
                }
            }
            ".type" => {
                if let Node((StrArg::Val(Some(label)), AsmStrArg::Val(Some(ty))), n) =
                    ctx.eval(self).coerced(n)
                {
                    let section = self.state_mut().expect_section(ctx.context, n);
                    let node = ctx.context.node_to_owned(n);
                    let mut section = self.state_mut().trans.resolve_mut(section);
                    let result = match ty.as_bytes() {
                        b"func" => section.set_symbol_ty(
                            label,
                            trans::sym::SymbolType::Function,
                            Some(node.clone()),
                        ),
                        b"obj" => section.set_symbol_ty(
                            label,
                            trans::sym::SymbolType::Object,
                            Some(node.clone()),
                        ),
                        b"data" => section.set_symbol_ty(
                            label,
                            trans::sym::SymbolType::Data,
                            Some(node.clone()),
                        ),
                        b"common" => section.set_symbol_ty(
                            label,
                            trans::sym::SymbolType::Common,
                            Some(node.clone()),
                        ),
                        _ => {
                            ctx.context
                                .report_error(n, format!("unknown symbol type '{ty}'"));
                            return;
                        }
                    };
                    if let Err(err) = result {
                        ctx.context.report_owned(err.to_log_entry(label, node));
                    }
                }
            }
            ".size" => {
                if let Node((StrArg::Val(Some(label)), UptrArg::Val(Some(size))), n) =
                    ctx.eval(self).coerced(n)
                {
                    let section = self.state_mut().expect_section(ctx.context, n);
                    let node = ctx.context.node_to_owned(n);
                    let result = self.state_mut().trans.resolve_mut(section).set_symbol_size(
                        label,
                        size.as_(),
                        Some(node.clone()),
                    );
                    if let Err(err) = result {
                        ctx.context.report_owned(err.to_log_entry(label, node));
                    }
                }
            }
            ".text" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".test", node);
            }
            ".bss" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".bss", node);
            }
            ".data" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".data", node);
            }
            ".rodata" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".rodata", node);
            }

            ".space" => {
                if let UptrArg::Val(Some(size)) = ctx.eval(self).coerced(n).0 {
                    self.add_space_data(ctx, size, num_traits::one(), n);
                }
            }
            ".values" => {
                for arg in ctx.eval(self).args(n, ArgumentsTypeHint::None).0 {
                    self.add_value_data(ctx, arg.0, arg.1);
                }
            }
            ".stringz" => {
                for Node(crate::expression::args::AsmStrArg::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(ctx, Constant::Str(arg.unwrap_or_default()), n);
                    if !matches!(arg, Some(AsmStr::CStr(_))) {
                        self.add_constant_data(ctx, Constant::U8(0), n);
                    }
                }
            }
            ".string" => {
                for Node(crate::expression::args::AsmStrArg::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(ctx, Constant::Str(arg.unwrap_or_default()), n);
                }
            }
            ".u8" => constant!(U8Arg, U8),
            ".u16" => constant!(U16Arg, U16),
            ".u32" => constant!(U32Arg, U32),
            ".u64" => constant!(U64Arg, U64),
            ".u128" => constant!(U128Arg, U128),
            ".i8" => constant!(I8Arg, I8),
            ".i16" => constant!(I16Arg, I16),
            ".i32" => constant!(I32Arg, I32),
            ".i64" => constant!(I64Arg, I64),
            ".i128" => constant!(I128Arg, I128),
            ".f32" => constant!(F32Arg, F32),
            ".f64" => constant!(F64Arg, F64),
            ".bool" => constant!(BoolArg, Bool),
            ".char" => constant!(CharArg, Char),
            ".iptr" => constant!(IptrArg, Iptr),
            ".isize" => constant!(IsizeArg, Isize),
            ".uptr" => constant!(UptrArg, Uptr),
            ".usize" => constant!(UsizeArg, Usize),

            _ => self.assemble_mnemonic(ctx, mnemonic, n),
        }
    }

    fn encounter_label(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mut label: &'a str,
        node: NodeRef<'a>,
    ) {
        if label.starts_with('.') {
            if let Some(prev) = self.state_mut().expect_last_label(ctx.context, node) {
                label = ctx.context.alloc_str(format!("{prev}{label}"))
            }
        } else {
            self.state_mut().last_non_local_label = Some(label);
        }

        {
            let section = self.state_mut().expect_section(ctx.context, node);
            let node = ctx.context.node_to_owned(node);
            let result = self
                .state_mut()
                .trans
                .resolve_mut(section)
                .bind_symbol(label, Some(node.clone()));

            if let Err(err) = result {
                ctx.context.report_owned(err.to_log_entry(label, node));
            }
        }

        self.encounter_label(ctx, label, node);
    }

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult {
        self.finish(ctx);

        //TODO bruh
        self.state_mut().trans.clone()
    }

    fn encounter_comment(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        comment: &'a str,
        n: NodeRef<'a>,
    ) {
        let section = self.state_mut().expect_section(ctx.context, n);
        let node = ctx.context.node_to_owned(n);
        self.state_mut()
            .trans
            .resolve_mut(section)
            .emit_comment_dbg(comment, node);
    }
}
