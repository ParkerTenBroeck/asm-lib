use assembler::{
    Node,
    expression::{ExprCtx, Value, ValueType, args::LabelArg},
};

use crate::{
    MipsAssembler,
    label::{LabelExpr, LabelExprType, RelocPattern},
};

impl<'a> MipsAssembler<'a> {
    pub(crate) fn eval_func_impl(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: assembler::expression::FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        match func.func() {
            "size" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    match l.ty {
                        LabelExprType::Empty => {}
                        LabelExprType::Unspecified(label) => l.ty = LabelExprType::Size(label),
                        LabelExprType::Sub(_, _) => ctx
                            .context
                            .report_error(n, "cannot set relocation on label subtraction"),
                        _ => ctx
                            .context
                            .report_error(n, "label relocation kind is already set"),
                    }
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "align" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    match l.ty {
                        LabelExprType::Empty => {}
                        LabelExprType::Unspecified(label) => l.ty = LabelExprType::Align(label),
                        LabelExprType::Sub(_, _) => ctx
                            .context
                            .report_error(n, "cannot set relocation on label subtraction"),
                        _ => ctx
                            .context
                            .report_error(n, "label relocation kind is already set"),
                    }
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "pcrel" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    match l.ty {
                        LabelExprType::Empty => {}
                        LabelExprType::Unspecified(label) => l.ty = LabelExprType::PcRel(label),
                        LabelExprType::Sub(_, _) => ctx
                            .context
                            .report_error(n, "cannot set relocation on label subtraction"),
                        _ => ctx
                            .context
                            .report_error(n, "label relocation kind is already set"),
                    }
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "absolute" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    match l.ty {
                        LabelExprType::Empty => {}
                        LabelExprType::Unspecified(label) => l.ty = LabelExprType::Absolute(label),
                        LabelExprType::Sub(_, _) => ctx
                            .context
                            .report_error(n, "cannot set relocation on label subtraction"),
                        _ => ctx
                            .context
                            .report_error(n, "label relocation kind is already set"),
                    }
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "hi" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(RelocPattern::High);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "lo" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(RelocPattern::Low);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u8" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(RelocPattern::U8);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u16" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(RelocPattern::U16);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u32" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(RelocPattern::U32);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            _ => ctx.eval(self).func_base(func, hint),
        }
    }
}
