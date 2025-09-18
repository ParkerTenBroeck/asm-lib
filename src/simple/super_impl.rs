use crate::expression::args::{IdentStrArg, RawIdentStrArg, UptrArg, UptrPow2Arg};
use crate::node::NodeTrait;
use crate::simple::trans::TranslationUnit;
use crate::simple::trans::sym::Symbol;
use crate::{
    assembler::LangCtx,
    context::{Node, NodeRef},
    expression::{
        ArgumentsTypeHint, AsmStr, Constant, ExprCtx, FuncParamParser, NodeVal, Value, ValueType,
        binop::BinOp, unop::UnOp,
    },
    lex::Number,
    util::IntoStrDelimable,
};

use num_traits::{AsPrimitive, WrappingSub};
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
        Node(mut ident, node): Node<'a, &'a str>,
        hint: crate::expression::ValueType<'a, Self>,
    ) -> crate::expression::Value<'a, Self> {
        if hint == ValueType::RawIdent {
            return Value::Ident(ident);
        }

        if ident.starts_with('.')
            && let Some(prev) = self.state_mut().expect_last_label(ctx.context, node)
        {
            ident = ctx.context.alloc_str(format!("{prev}{}", ident))
        }

        if hint == ValueType::Ident {
            return Value::Ident(ident);
        }
        match ident {
            "__line__" => Value::Constant(Constant::U32(node.top().span.line.wrapping_add(1))),
            "__col__" => Value::Constant(Constant::U32(node.top().span.col)),
            "__len__" => Value::Constant(Constant::U32(node.top().span.len)),
            "__offset__" => Value::Constant(Constant::U32(node.top().span.offset)),
            "__file__" => Value::Constant(Constant::Str(
                if let Some(str) = node.top().source.path.as_os_str().to_str() {
                    AsmStr::Str(str)
                } else {
                    AsmStr::ByteStr(node.top().source.path.as_os_str().as_bytes())
                },
            )),
            "__local_line__" => Value::Constant(Constant::U32(node.span.line.wrapping_add(1))),
            "__local_col__" => Value::Constant(Constant::U32(node.span.col)),
            "__local_len__" => Value::Constant(Constant::U32(node.span.len)),
            "__local_offset__" => Value::Constant(Constant::U32(node.span.offset)),
            "__local_file__" => Value::Constant(Constant::Str(
                if let Some(str) = node.source.path.as_os_str().to_str() {
                    AsmStr::Str(str)
                } else {
                    AsmStr::ByteStr(node.source.path.as_os_str().as_bytes())
                },
            )),
            _ => self.parse_ident(ctx, Node(ident, node), hint),
        }
    }

    fn parse_numeric_literal(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        num: Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        if matches!(num.0.get_suffix(), Some("f")) {
            let label =
                self.state_mut()
                    .expect_front_numeric_label(ctx.context, num.1, num.0.get_num());
            if let Some(label) = label {
                self.parse_ident(ctx, Node(label, num.1), hint)
            } else {
                Value::Label(T::Label::default())
            }
        } else if matches!(num.0.get_suffix(), Some("b")) {
            let label =
                self.state_mut()
                    .expect_back_numeric_label(ctx.context, num.1, num.0.get_num());
            if let Some(label) = label {
                self.parse_ident(ctx, Node(label, num.1), hint)
            } else {
                Value::Label(T::Label::default())
            }
        } else {
            self.parse_numeric_literal(ctx, num, negated, hint)
        }
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

            ".label" => {
                if let Node(IdentStrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {
                    self.encounter_label(ctx, label, n);
                }
            }
            ".global" | ".weak" | ".local" => {
                let Node(args, node) = ctx.eval(self).coerced(n);
                let label = match args {
                    None => {
                        if let Some(label) = self.state_mut().expect_last_label(ctx.context, node) {
                            label
                        } else {
                            return;
                        }
                    }
                    Some(IdentStrArg::Val(Some(label))) => label,
                    _ => return,
                };
                let node_owned = ctx.context.node_to_owned(node);
                let result = self.state_mut().trans.set_symbol_visibility(
                    label,
                    match mnemonic {
                        ".global" => trans::sym::SymbolVisibility::Global,
                        ".weak" => trans::sym::SymbolVisibility::Weak,
                        ".local" => trans::sym::SymbolVisibility::Local,
                        _ => unreachable!(),
                    },
                    Some(node_owned.clone()),
                );
                if let Err(err) = result {
                    ctx.context
                        .report_owned(err.to_log_entry(label, node_owned));
                }
            }
            ".align" => {
                let Node(UptrPow2Arg::Val(align), node) = ctx.eval(self).coerced(n);
                if let Some(align) = align {
                    self.add_align(ctx, align, node);
                }
            }
            ".type" => {
                let Node(args, node) = ctx.eval(self).coerced(n);
                let (ty, label) = match args {
                    (RawIdentStrArg::Val(Some(ty)), None) => (
                        ty,
                        if let Some(label) = self.state_mut().expect_last_label(ctx.context, node) {
                            label
                        } else {
                            return;
                        },
                    ),
                    (RawIdentStrArg::Val(Some(ty)), Some(IdentStrArg::Val(Some(label)))) => {
                        (ty, label)
                    }
                    _ => return,
                };
                let node_owned = ctx.context.node_to_owned(node);
                let result = match ty.as_bytes() {
                    b"func" => self.state_mut().trans.set_symbol_ty(
                        label,
                        trans::sym::SymbolType::Function,
                        Some(node_owned.clone()),
                    ),
                    b"obj" => self.state_mut().trans.set_symbol_ty(
                        label,
                        trans::sym::SymbolType::Object,
                        Some(node_owned.clone()),
                    ),
                    b"data" => self.state_mut().trans.set_symbol_ty(
                        label,
                        trans::sym::SymbolType::Data,
                        Some(node_owned.clone()),
                    ),
                    b"common" => self.state_mut().trans.set_symbol_ty(
                        label,
                        trans::sym::SymbolType::Common,
                        Some(node_owned.clone()),
                    ),
                    _ => {
                        ctx.context
                            .report_error(n, format!("unknown symbol type '{ty}'"));
                        return;
                    }
                };
                if let Err(err) = result {
                    ctx.context
                        .report_owned(err.to_log_entry(label, node_owned));
                }
            }
            ".size" => {
                let Node((size, label), node): Node<(
                    Option<UptrArg<'a, _>>,
                    Option<IdentStrArg<'a, _>>,
                )> = ctx.eval(self).coerced(n);

                let label = if let Some(IdentStrArg::Val(label)) = label {
                    if let Some(label) = label {
                        label
                    } else {
                        return;
                    }
                } else if let Some(label) = self.state_mut().expect_last_label(ctx.context, node) {
                    label
                } else {
                    return;
                };
                let size = if let Some(UptrArg::Val(size)) = size {
                    size.unwrap_or_default()
                } else {
                    let Some(symbol) = self.state_mut().trans.resolve_symbol(label) else {
                        ctx.context.report_error(
                            node,
                            "cannot set implicit size on symbol that has not been bound",
                        );
                        return;
                    };
                    let Some(Symbol {
                        section: Some(section_idx),
                        offset,
                        ..
                    }) = self.state_mut().trans.get_symbol_mut(symbol).copied()
                    else {
                        ctx.context.report_error(
                            node,
                            "cannot set implicit size on symbol that has not been bound",
                        );
                        return;
                    };

                    self.state_mut()
                        .trans
                        .get(section_idx)
                        .data
                        .current_offset()
                        .wrapping_sub(&offset)
                };
                let node_owned = ctx.context.node_to_owned(node);
                let result = self.state_mut().trans.set_symbol_size(
                    label,
                    size.as_(),
                    Some(node_owned.clone()),
                );
                if let Err(err) = result {
                    ctx.context
                        .report_owned(err.to_log_entry(label, node_owned));
                }
            }

            ".section" => {
                if let Node(RawIdentStrArg::Val(Some(sec)), node) = ctx.eval(self).coerced(n) {
                    self.set_section(ctx, sec, node);
                }
            }
            ".text" | ".bss" | ".data" | ".rodata" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, mnemonic, node);
            }

            ".space" => {
                if let Node(UptrArg::Val(Some(size)), node) = ctx.eval(self).coerced(n) {
                    self.add_space_data(ctx, size, num_traits::one(), node);
                }
            }
            ".values" => {
                for arg in ctx.eval(self).args(n, ArgumentsTypeHint::None).0 {
                    self.add_value_data(ctx, arg.0, arg.1);
                }
            }
            ".cstring" => {
                for Node(crate::expression::args::CStrArg::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(
                        ctx,
                        Constant::Str(AsmStr::CStr(arg.unwrap_or_default())),
                        n,
                    );
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
            ".ifunc" => constant!(IfuncArg, Ifunc),
            ".uptr" => constant!(UptrArg, Uptr),
            ".usize" => constant!(UsizeArg, Usize),
            ".ufunc" => constant!(UfuncArg, Ufunc),

            _ => self.assemble_mnemonic(ctx, mnemonic, n),
        }
    }

    fn encounter_label(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        label: &'a str,
        node: NodeRef<'a>,
    ) {
        let (label, local) = if label.starts_with(".") {
            if let Some(prev) = self.state_mut().expect_last_label(ctx.context, node) {
                (ctx.context.alloc_str(format!("{prev}{label}")), true)
            } else {
                (label, true)
            }
        } else if label.starts_with(char::is_numeric) {
            let Some(label) =
                self.state_mut()
                    .expect_next_local_numeric_label(ctx.context, node, label)
            else {
                return;
            };
            (label, true)
        } else {
            self.state_mut().last_non_local_label = Some(label);
            (label, false)
        };

        let node_owned = ctx.context.node_to_owned(node);
        let result = self
            .current_section_mut(ctx, node)
            .bind_symbol_resolve(label, Some(node_owned.clone()));

        if let Err(err) = result {
            ctx.context
                .report_owned(err.to_log_entry(label, node_owned.clone()));
        }

        if local {
            let result = self.state_mut().trans.set_symbol_visibility(
                label,
                trans::sym::SymbolVisibility::Local,
                Some(node_owned.clone()),
            );
            if let Err(err) = result {
                ctx.context
                    .report_owned(err.to_log_entry(label, node_owned));
            }
        }

        self.encounter_label(ctx, label, node);
    }

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult {
        self.finish(ctx);

        let mut other = TranslationUnit::new();
        std::mem::swap(&mut self.state_mut().trans, &mut other);
        other
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
