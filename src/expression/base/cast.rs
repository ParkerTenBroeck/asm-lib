use crate::expression::{AsmStr, ValueType};
use crate::{
    assembler::lang::AssemblyLanguage,
    context::{Node, NodeRef},
    expression::{Constant, ExpressionEvaluator, NodeVal, Value},
};

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn cast_base(
        &mut self,
        node: NodeRef<'a>,
        expr: NodeVal<'a, L>,
        _: NodeRef<'a>,
        ty: Node<'a, &'a str>,
        _: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let ty = ty.0;

        macro_rules! numeric_cast {
            ($ident:ident $(, $additional:ident)*) => {{
                use crate::expression::conversion::FromAsPrimitive;
                use Constant as C;
                use Value::Constant as V_C;
                match expr.0 {
                    V_C(C::I8(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I16(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I32(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I64(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I128(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Isize(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Iptr(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Ifunc(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U8(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U16(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U32(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U64(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U128(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Usize(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Uptr(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Ufunc(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::F32(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::F64(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    $(
                        V_C(C::$additional(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    )*
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            }};
        }

        use num_traits::AsPrimitive;
        match ty {
            "str" => Value::Constant(Constant::Str(AsmStr::Str(match expr.0 {
                Value::Constant(Constant::Str(AsmStr::Str(str))) => str,
                Value::Constant(Constant::Str(bytes)) => match str::from_utf8(bytes.as_bytes()) {
                    Ok(str) => str,
                    Err(_) => {
                        self.context
                            .report_error(node, "contents of byte/c string contain bytes which cannot be present in ordinary strings");
                        ""
                    }
                },
                other => self.context.alloc_str(format!("{other}")),
            }))),
            "cstr" => Value::Constant(Constant::Str(AsmStr::CStr(match expr.0 {
                Value::Constant(Constant::Str(str)) => str.as_bytes(),
                other => self.context.alloc_str(format!("{other}")).as_bytes(),
            }))),
            "bstr" => Value::Constant(Constant::Str(AsmStr::ByteStr(match expr.0 {
                Value::Constant(Constant::Str(str)) => str.as_bytes(),
                other => self.context.alloc_str(format!("{other}")).as_bytes(),
            }))),
            "i8" => numeric_cast!(I8, Bool, Char),
            "i16" => numeric_cast!(I16, Bool, Char),
            "i32" => numeric_cast!(I32, Bool, Char),
            "i64" => numeric_cast!(I64, Bool, Char),
            "i128" => numeric_cast!(I128, Bool, Char),
            "isize" => numeric_cast!(Isize, Bool, Char),
            "iptr" => numeric_cast!(Iptr, Bool, Char),
            "ifunc" => numeric_cast!(Ifunc, Bool, Char),
            "u8" => numeric_cast!(U8, Bool, Char),
            "u16" => numeric_cast!(U16, Bool, Char),
            "u32" => numeric_cast!(U32, Bool, Char),
            "u64" => numeric_cast!(U64, Bool, Char),
            "u128" => numeric_cast!(U128, Bool, Char),
            "usize" => numeric_cast!(Usize, Bool, Char),
            "uptr" => numeric_cast!(Uptr, Bool, Char),
            "ufunc" => numeric_cast!(Ufunc, Bool, Char),
            "f32" => numeric_cast!(F32),
            "f64" => numeric_cast!(F64),
            "char" => match expr.0 {
                Value::Constant(Constant::U8(i)) => Value::Constant(Constant::Char(i as char)),
                Value::Constant(Constant::U16(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::U32(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::U64(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::Uptr(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i.as_()).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::Usize(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i.as_()).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::Char(i)) => Value::Constant(Constant::Char(i)),
                _ => self.cast_error(expr, ValueType::Char),
            },
            _ => {
                self.context
                    .report_error(node, format!("unknown type '{ty}'"));
                expr.0
            }
        }
    }

    pub fn cast_error(&mut self, expr: NodeVal<'a, L>, expected: ValueType<'a, L>) -> Value<'a, L> {
        self.context.report_error(
            expr.1,
            format!("cannot cast '{}' to '{}'", expr.0.get_type(), expected),
        );
        expected.default_value()
    }
}
