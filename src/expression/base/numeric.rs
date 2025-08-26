use crate::{
    assembler::lang::AssemblyLanguage,
    context::Node,
    expression::{Constant, ExpressionEvaluator, Value, ValueType},
    lex::{Number, TypeHint},
};

use std::num::IntErrorKind;

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn parse_numeric_literal_base(
        &mut self,
        Node(num, n): Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let mut buf = [0u8; 32];
        let suffix = if let Some(suffix) = num.get_suffix() {
            let mut index = 0;
            for c in suffix.chars() {
                if c != '_' {
                    if index + c.len_utf8() > buf.len() {
                        self.context
                            .report_error(n, format!("unknown numeric suffix '{suffix}'"));
                        return hint.default_value();
                    }
                    c.encode_utf8(&mut buf[index..]);
                    index += c.len_utf8();
                }
            }
            Some(str::from_utf8(&buf[..index]).unwrap())
        } else {
            None
        };

        let (suffix, radix) = match num.get_hint() {
            TypeHint::Float if hint.is_integer() => {
                (suffix.unwrap_or(L::DEFAULT_FLOAT_POSTFIX), 10)
            }
            TypeHint::Float => (
                suffix.unwrap_or(hint.numeric_suffix().unwrap_or(L::DEFAULT_FLOAT_POSTFIX)),
                10,
            ),
            TypeHint::Hex => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                16,
            ),
            TypeHint::Bin => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                2,
            ),
            TypeHint::Int => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                10,
            ),
            TypeHint::Oct => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                8,
            ),
        };

        let negated = suffix.starts_with("i") && negated;

        let mut buf = [0u8; 256];
        let number = {
            let mut index = 0;
            if negated {
                buf[index] = b'-';
                index += 1;
            }

            for c in num.get_num().chars() {
                if c != '_' {
                    if index + c.len_utf8() > buf.len() {
                        self.context.report_error(n, "numeric literal too long");
                        return hint.default_value();
                    }
                    c.encode_utf8(&mut buf[index..]);
                    index += c.len_utf8();
                }
            }
            str::from_utf8(&buf[..index]).unwrap()
        };

        use num_traits::Bounded;
        use num_traits::Num;
        use num_traits::WrappingNeg;
        macro_rules! integer {
            ($num:ty) => {{
                let v = <$num>::from_str_radix(number, radix)
                    .inspect_err(|e| {
                        use crate::LogEntry;
                        match e.kind(){
                            IntErrorKind::NegOverflow | IntErrorKind::PosOverflow => {
                                let smaller_larger = match e.kind() {
                                    IntErrorKind::NegOverflow => "small",
                                    IntErrorKind::PosOverflow => "large",
                                    _ => "???????",
                                };
                                #[allow(clippy::legacy_numeric_constants)]
                                let mut error = LogEntry::new()
                                        .error(n, format!("numeric literal too {smaller_larger} to fit in '{suffix}' valid range is {}..={}", <$num>::min_value(), <$num>::max_value()));

                                if num.get_suffix().is_none() && hint == ValueType::Any {
                                    error = error.hint_locless(format!("consider adding an explicit type suffix to the literal like {}{}i64{}", num.get_num(), crate::logs::GREEN, crate::logs::RESET))
                                }

                                self.context.report(error);

                            }
                            _ => self.context.report_error(n, format!("invalid numeric literal"))
                        }
                    })
                    .unwrap_or(num_traits::zero());
                if negated{
                    v.wrapping_neg()
                }else{
                    v
                }
            }};
        }

        macro_rules! float {
            ($num:ty) => {
                number
                    .parse()
                    .inspect_err(|e| {
                        self.context
                            .report_error(n, format!("invalid numeric literal '{e}'"));
                    })
                    .unwrap_or(0.0)
            };
        }

        Value::Constant(match suffix {
            "i8" => Constant::I8(integer!(i8)),
            "i16" => Constant::I16(integer!(i16)),
            "i32" => Constant::I32(integer!(i32)),
            "i64" => Constant::I64(integer!(i64)),
            "i128" => Constant::I128(integer!(i128)),
            "isize" => Constant::Isize(integer!(L::Isize)),
            "iptr" => Constant::Iptr(integer!(L::Iptr)),

            "u8" => Constant::U8(integer!(u8)),
            "u16" => Constant::U16(integer!(u16)),
            "u32" => Constant::U32(integer!(u32)),
            "u64" => Constant::U64(integer!(u64)),
            "u128" => Constant::U128(integer!(u128)),
            "usize" => Constant::Usize(integer!(L::Usize)),
            "uptr" => Constant::Uptr(integer!(L::Uptr)),

            "f32" => Constant::F32(float!(f32)),
            "f64" => Constant::F64(float!(f64)),

            suffix => {
                self.context
                    .report_error(n, format!("unknown numeric suffix '{suffix}'"));
                return hint.default_value();
            }
        })
    }
}
