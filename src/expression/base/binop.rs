use num_traits::*;

use crate::{
    assembler::lang::AssemblyLanguage,
    context::{Node, NodeRef},
    expression::{Constant, ExpressionEvaluator, NodeVal, Value, ValueType},
};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    Xor,
    Or,
    Shl,
    Shr,

    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Ne,
}

impl BinOp {
    pub fn precedence(&self) -> u32 {
        match self {
            BinOp::Add => 20 - 4,
            BinOp::Sub => 20 - 4,

            BinOp::Mul => 20 - 3,
            BinOp::Div => 20 - 3,
            BinOp::Rem => 20 - 3,

            BinOp::Shl => 20 - 5,
            BinOp::Shr => 20 - 5,

            BinOp::Lt => 20 - 6,
            BinOp::Lte => 20 - 6,
            BinOp::Gt => 20 - 6,
            BinOp::Gte => 20 - 6,

            BinOp::Eq => 20 - 7,
            BinOp::Ne => 20 - 7,

            BinOp::And => 20 - 8,
            BinOp::Xor => 20 - 9,
            BinOp::Or => 20 - 10,
        }
    }
}

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn binop_base(
        &mut self,
        node: NodeRef<'a>,
        lhs: NodeVal<'a, L>,
        op: Node<'a, BinOp>,
        rhs: NodeVal<'a, L>,
        hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let op = op.0;

        macro_rules! constants_grouped {
            ($l:ident, $r:ident, $($integer:block)?, $($float:block)?, $($string:block)?, $($char:block)?, $($bool:block)?) => {
                'label: {Value::Constant(match ($l, $r){
                    $(
                        (Constant::I8($l), Constant::I8($r)) => Constant::I8($integer),
                        (Constant::I16($l), Constant::I16($r)) => Constant::I16($integer),
                        (Constant::I32($l), Constant::I32($r)) => Constant::I32($integer),
                        (Constant::I64($l), Constant::I64($r)) => Constant::I64($integer),
                        (Constant::I128($l), Constant::I128($r)) => Constant::I128($integer),
                        (Constant::Isize($l), Constant::Isize($r)) => Constant::Isize($integer),
                        (Constant::Iptr($l), Constant::Iptr($r)) => Constant::Iptr($integer),
                        (Constant::Ifunc($l), Constant::Ifunc($r)) => Constant::Ifunc($integer),
                        (Constant::U8($l), Constant::U8($r)) => Constant::U8($integer),
                        (Constant::U16($l), Constant::U16($r)) => Constant::U16($integer),
                        (Constant::U32($l), Constant::U32($r)) => Constant::U32($integer),
                        (Constant::U64($l), Constant::U64($r)) => Constant::U64($integer),
                        (Constant::U128($l), Constant::U128($r)) => Constant::U128($integer),
                        (Constant::Usize($l), Constant::Usize($r)) => Constant::Usize($integer),
                        (Constant::Uptr($l), Constant::Uptr($r)) => Constant::Uptr($integer),
                        (Constant::Ufunc($l), Constant::Ufunc($r)) => Constant::Ufunc($integer),
                    )?
                    $(
                        (Constant::F32($l), Constant::F32($r)) => Constant::F32($float),
                        (Constant::F64($l), Constant::F64($r)) => Constant::F64($float),
                    )?
                    $((Constant::String($l), Constant::String($r)) => Constant::String($string),)?
                    $((Constant::Char($l), Constant::Char($r)) => Constant::Char($char),)?
                    $((Constant::Bool($l), Constant::Bool($r)) => Constant::Bool($bool),)?

                    _ => break 'label (self.invalid_binop(op, node, lhs, rhs, hint))
                })}
            };
        }

        macro_rules! constant_cmp {
            ($l:ident, $r:ident, $block:block) => {
                'label: {
                    Value::Constant(match ($l, $r) {
                        (Constant::I8($l), Constant::I8($r)) => Constant::Bool($block),
                        (Constant::I16($l), Constant::I16($r)) => Constant::Bool($block),
                        (Constant::I32($l), Constant::I32($r)) => Constant::Bool($block),
                        (Constant::I64($l), Constant::I64($r)) => Constant::Bool($block),
                        (Constant::I128($l), Constant::I128($r)) => Constant::Bool($block),
                        (Constant::Isize($l), Constant::Isize($r)) => Constant::Bool($block),
                        (Constant::Iptr($l), Constant::Iptr($r)) => Constant::Bool($block),
                        (Constant::Ifunc($l), Constant::Ifunc($r)) => Constant::Bool($block),
                        (Constant::U8($l), Constant::U8($r)) => Constant::Bool($block),
                        (Constant::U16($l), Constant::U16($r)) => Constant::Bool($block),
                        (Constant::U32($l), Constant::U32($r)) => Constant::Bool($block),
                        (Constant::U64($l), Constant::U64($r)) => Constant::Bool($block),
                        (Constant::U128($l), Constant::U128($r)) => Constant::Bool($block),
                        (Constant::Usize($l), Constant::Usize($r)) => Constant::Bool($block),
                        (Constant::Uptr($l), Constant::Uptr($r)) => Constant::Bool($block),
                        (Constant::Ufunc($l), Constant::Ufunc($r)) => Constant::Bool($block),
                        (Constant::F32($l), Constant::F32($r)) => Constant::Bool($block),
                        (Constant::F64($l), Constant::F64($r)) => Constant::Bool($block),
                        (Constant::Char($l), Constant::Char($r)) => Constant::Bool($block),
                        (Constant::Bool($l), Constant::Bool($r)) => Constant::Bool($block),
                        _ => break 'label (self.invalid_binop(op, node, lhs, rhs, hint)),
                    })
                }
            };
        }

        match op {
            BinOp::Add => match (lhs.0, rhs.0) {
                (Value::Constant(Constant::Str(l)), r) => Value::Constant(Constant::Str(
                    self.context.alloc_str(format!("{l}{r}")).into(),
                )),
                (l, Value::Constant(Constant::Str(r))) => Value::Constant(Constant::Str(
                    self.context.alloc_str(format!("{l}{r}")).into(),
                )),

                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{WrappingAdd::wrapping_add(&l, &r)},/*float*/{l+r},/*str*/,/*char*/,/*bool*/
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Sub => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{WrappingSub::wrapping_sub(&l, &r)},/*float*/{l-r},/*str*/,/*char*/,/*bool*/
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Mul => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{WrappingMul::wrapping_mul(&l, &r)},/*float*/{l*r},/*str*/,/*char*/,/*bool*/
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Div => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{
                        if r!=num_traits::zero(){
                            l / r
                        }else{
                            self.context.report_error(node, "divide by zero");
                            num_traits::zero()
                        }
                    },/*float*/{l/r},/*str*/,/*char*/,/*bool*/
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Rem => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{
                        if r!=num_traits::zero(){
                            l % r
                        }else{
                            self.context.report_error(node, "remainder by zero");
                            num_traits::zero()
                        }
                    },/*float*/{l%r},/*str*/,/*char*/,/*bool*/
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Xor => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l^r},/*float*/,/*str*/,/*char*/,/*bool*/{l^r}
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::And => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l&r},/*float*/,/*str*/,/*char*/,/*bool*/{l&r}
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Or => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l|r},/*float*/,/*str*/,/*char*/,/*bool*/{l|r}
                ),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Shl => 'label: {
                match (lhs.0, rhs.0) {
                    (Value::Constant(l), Value::Constant(r)) => Value::Constant({
                        let Some(c) = r.checked_cast_u32_with(
                            rhs.1,
                            self.context,
                            self.context.config().implicit_cast_shift_value,
                        ) else {
                            break 'label self.invalid_binop(op, node, lhs, rhs, hint);
                        };
                        match l {
                            Constant::I8(l) => Constant::I8(l.wrapping_shl(c)),
                            Constant::I16(l) => Constant::I16(l.wrapping_shl(c)),
                            Constant::I32(l) => Constant::I32(l.wrapping_shl(c)),
                            Constant::I64(l) => Constant::I64(l.wrapping_shl(c)),
                            Constant::I128(l) => Constant::I128(l.wrapping_shl(c)),
                            Constant::Isize(l) => Constant::Isize(l.wrapping_shl(c)),
                            Constant::Iptr(l) => Constant::Iptr(l.wrapping_shl(c)),
                            Constant::U8(l) => Constant::U8(l.wrapping_shl(c)),
                            Constant::U16(l) => Constant::U16(l.wrapping_shl(c)),
                            Constant::U32(l) => Constant::U32(l.wrapping_shl(c)),
                            Constant::U64(l) => Constant::U64(l.wrapping_shl(c)),
                            Constant::U128(l) => Constant::U128(l.wrapping_shl(c)),
                            Constant::Usize(l) => Constant::Usize(l.wrapping_shl(c)),
                            Constant::Uptr(l) => Constant::Uptr(l.wrapping_shl(c)),

                            _ => break 'label self.invalid_binop(op, node, lhs, rhs, hint),
                        }
                    }),
                    _ => self.invalid_binop(op, node, lhs, rhs, hint),
                }
            }
            BinOp::Shr => 'label: {
                match (lhs.0, rhs.0) {
                    (Value::Constant(l), Value::Constant(r)) => Value::Constant({
                        let Some(c) = r.checked_cast_u32_with(
                            rhs.1,
                            self.context,
                            self.context.config().implicit_cast_shift_value,
                        ) else {
                            break 'label self.invalid_binop(op, node, lhs, rhs, hint);
                        };

                        match l {
                            Constant::I8(l) => Constant::I8(l.wrapping_shr(c)),
                            Constant::I16(l) => Constant::I16(l.wrapping_shr(c)),
                            Constant::I32(l) => Constant::I32(l.wrapping_shr(c)),
                            Constant::I64(l) => Constant::I64(l.wrapping_shr(c)),
                            Constant::I128(l) => Constant::I128(l.wrapping_shr(c)),
                            Constant::Isize(l) => Constant::Isize(l.wrapping_shr(c)),
                            Constant::Iptr(l) => Constant::Iptr(l.wrapping_shr(c)),
                            Constant::U8(l) => Constant::U8(l.wrapping_shr(c)),
                            Constant::U16(l) => Constant::U16(l.wrapping_shr(c)),
                            Constant::U32(l) => Constant::U32(l.wrapping_shr(c)),
                            Constant::U64(l) => Constant::U64(l.wrapping_shr(c)),
                            Constant::U128(l) => Constant::U128(l.wrapping_shr(c)),
                            Constant::Usize(l) => Constant::Usize(l.wrapping_shr(c)),
                            Constant::Uptr(l) => Constant::Uptr(l.wrapping_shr(c)),

                            _ => break 'label self.invalid_binop(op, node, lhs, rhs, hint),
                        }
                    }),
                    _ => self.invalid_binop(op, node, lhs, rhs, hint),
                }
            }

            BinOp::Lt => match (lhs.0, rhs.0) {
                #[allow(clippy::bool_comparison)]
                (Value::Constant(l), Value::Constant(r)) => constant_cmp!(l, r, { l < r }),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Lte => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constant_cmp!(l, r, { l <= r }),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Gt => match (lhs.0, rhs.0) {
                #[allow(clippy::bool_comparison)]
                (Value::Constant(l), Value::Constant(r)) => constant_cmp!(l, r, { l > r }),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Gte => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constant_cmp!(l, r, { l >= r }),
                _ => self.invalid_binop(op, node, lhs, rhs, hint),
            },
            BinOp::Eq => {
                if lhs.0.get_type() == rhs.0.get_type() {
                    Value::Constant(Constant::Bool(lhs.0 == rhs.0))
                } else {
                    self.invalid_binop(op, node, lhs, rhs, hint)
                }
            }
            BinOp::Ne => {
                if lhs.0.get_type() == rhs.0.get_type() {
                    Value::Constant(Constant::Bool(lhs.0 != rhs.0))
                } else {
                    self.invalid_binop(op, node, lhs, rhs, hint)
                }
            }
        }
    }

    pub fn invalid_binop(
        &mut self,
        op: BinOp,
        node: NodeRef<'a>,
        lhs: NodeVal<'a, L>,
        rhs: NodeVal<'a, L>,
        _: ValueType<'a, L>,
    ) -> Value<'a, L> {
        match op {
            BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::Ne => {
                self.context.report_error(
                    node,
                    format!(
                        "cannot compare types '{}' and '{}'",
                        lhs.0.get_type(),
                        rhs.0.get_type()
                    ),
                );
                Value::Constant(Constant::Bool(lhs.0 != rhs.0))
            }

            _ => {
                self.context.report_error(
                    node,
                    format!(
                        "cannot '{:?}' types '{}' and '{}'",
                        op,
                        lhs.0.get_type(),
                        rhs.0.get_type()
                    ),
                );
                lhs.0
            }
        }
    }

    pub fn invalid_index(
        &mut self,
        node: NodeRef<'a>,
        lhs: Option<NodeVal<'a, L>>,
        rhs: Option<NodeVal<'a, L>>,
    ) -> Value<'a, L> {
        match (lhs, rhs) {
            (None, None) => self.context.report_error(node, "cannot index with nothing"),
            (None, Some(rhs)) => self
                .context
                .report_error(node, format!("cannot index with '{}'", rhs.0.get_type())),
            (Some(lhs), None) => self
                .context
                .report_error(node, format!("cannot index '{}''", lhs.0.get_type(),)),

            (Some(lhs), Some(rhs)) => self.context.report_error(
                node,
                format!(
                    "cannot index '{}' with '{}'",
                    lhs.0.get_type(),
                    rhs.0.get_type()
                ),
            ),
        }
        Value::Indexed(L::Indexed::default())
    }

    pub fn index_base(
        &mut self,
        node: NodeRef<'a>,
        lhs: Option<NodeVal<'a, L>>,
        _opening: NodeRef<'a>,
        rhs: Option<NodeVal<'a, L>>,
        _closing: NodeRef<'a>,
        _hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        self.invalid_index(node, lhs, rhs)
    }
}
