use std::fmt::Write;

use crate::{
    assembler::lang::AssemblyLanguage,
    context::Node,
    expression::{
        AsmString, Constant, ExpressionEvaluator, FuncParamParser, Value, ValueType, WriteStrError,
        args::{AsmStrArg, PathArg},
    },
    logs::LogEntry,
};

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn func_base(
        &mut self,
        func: FuncParamParser<'a, 'b>,
        hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let (lang, mut ctx) = self.split_ctx();
        match func.func() {
            "format" => {
                if let Node((Node(AsmStrArg::Val(Some(format)), formt_n), v), n) =
                    func.coerced_args::<(_, Vec<Node<'a, Value<'a, L>>>), _>(lang, &mut ctx)
                {
                    let mut result = AsmString::new(format.kind());
                    let count = format.split('%').count() - 1;
                    if count - 1 != v.len() {
                        ctx.context.report_error(
                            n,
                            format!(
                                "wrong number of arguments provided expected {} found {}",
                                count,
                                v.len()
                            ),
                        )
                    }
                    let mut iter = v.into_iter();
                    for part in format.split('%') {
                        if let Some(Node(value, vn)) = iter.next() {
                            match value {
                                Value::Constant(Constant::Str(str)) => {
                                    match result.write_asm_str(str) {
                                        Ok(_) => {}
                                        Err(WriteStrError::CannotWriteByteStrToRegularString) => {
                                            use crate::ansi::*;
                                            ctx.context.report(
                                                LogEntry::new().error(vn, "byte strings can contain values which are not permitted in ordinary strings")
                                                .hint(formt_n, format!("consider adding leading `b` {GREEN}b{RESET}{}", formt_n.src_slice(),))
                                            );
                                        }
                                        Err(WriteStrError::CannotWriteCStrToRegularString) => {
                                            use crate::ansi::*;
                                            ctx.context.report(
                                                LogEntry::new().error(vn, "c strings can contain values which are not permitted in ordinary strings")
                                                .hint(formt_n, format!("consider adding leading `c` {GREEN}c{RESET}{}", formt_n.src_slice(),))
                                            );
                                        }
                                    }
                                }
                                _ => _ = result.write_fmt(format_args!("{value}")),
                            }
                        }
                        match result.write_asm_str(part) {
                            Ok(_) => {}
                            Err(_) => unreachable!(),
                        }
                    }

                    Value::Constant(Constant::Str(result.alloc_str(ctx.context)))
                } else {
                    Value::Constant(Constant::Str(Default::default()))
                }
            }
            "include_bytes" => {
                if let Node(PathArg::Val(Some(path)), n) = func.coerced_args(lang, &mut ctx) {
                    match ctx.context.get_bin(path) {
                        Ok(bytes) => Value::Constant(Constant::Str(
                            crate::expression::AsmStr::ByteStr(bytes),
                        )),
                        Err(err) => {
                            ctx.context
                                .report_error(n, format!("failed to include bytes: {err}"));
                            Value::Constant(Constant::Str(crate::expression::AsmStr::ByteStr(&[])))
                        }
                    }
                } else {
                    Value::Constant(Constant::Str(crate::expression::AsmStr::ByteStr(&[])))
                }
            }
            "include_str" => {
                if let Node(PathArg::Val(Some(path)), n) = func.coerced_args(lang, &mut ctx) {
                    match ctx.context.get_text(path) {
                        Ok(str) => Value::Constant(Constant::Str(crate::expression::AsmStr::Str(
                            str.contents,
                        ))),
                        Err(err) => {
                            ctx.context
                                .report_error(n, format!("failed to include str: {err}"));
                            Value::Constant(Constant::Str(Default::default()))
                        }
                    }
                } else {
                    Value::Constant(Constant::Str(Default::default()))
                }
            }
            "size_of" => {
                let val: Value<'a, L> = func.coerced_args(lang, &mut ctx).0;
                Value::Constant(Constant::Uptr(val.get_size().unwrap_or(num_traits::one())))
            }
            "align_of" => {
                let val: Value<'a, L> = func.coerced_args(lang, &mut ctx).0;
                Value::Constant(Constant::Uptr(val.get_align().unwrap_or(num_traits::one())))
            }
            "type_of" => {
                let val: Value<'a, L> = func.coerced_args(lang, &mut ctx).0;
                Value::Type(val.get_type())
            }
            _ => self.invalid_func(func, hint),
        }
    }

    pub fn invalid_func(
        &mut self,
        func: FuncParamParser<'a, 'b>,
        hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let func_name = func.func.0;
        let (lang, mut ctx) = self.split_ctx();
        let node = func.args(lang, &mut ctx).1;
        self.context
            .report_error(node, format!("unknown function {func_name}"));
        hint.default_value()
    }
}
