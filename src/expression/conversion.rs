use crate::AssemblyLanguage;
use crate::expression::AsmStr;
use crate::expression::Constant;
use crate::expression::ValueType;
use crate::logs::LogEntry;
use crate::logs::LogKind;
use num_traits::*;

macro_rules! implicit_convert_into {
    (
        into: $into:ty,
        $(from_signed: $from_signed:ty,)?
        $(from_unsigned: $from_unsigned:ty,)?
        $(fnarrow: $fnarrow:ty,)?
        $(fwide: $fwide:ty,)?
        $(try_narrowing: [$($try_narrowing:ty),*],)?
        $(try_widening: [$($try_widening:ty),*],)?
        $(narrowing: [$($narrowing:ty),*],)?
        $(widening: [$($widening:ty),*],)?
        $(changed: [$($changed:ty),*],)?
        $(f2i: [$($f2i:ty),*],)?
        $(i2f: [$($i2f:ty),*],)?
        $(inconvertable: [$($inconvertable:ty),*],)?
        $(asmstr: [$($asmstr:ty),*])?
        $(,)?
    ) => {
        $(
            impl ConvertableFrom<$from_unsigned> for $into{
                fn convert_from(from: $from_unsigned) -> ConversionResult<$into>{
                    match from.try_into(){
                        Ok(ok) => ConversionResult::Changed(ConversionChange::UnsignedToSigned(ok)),
                        Err(_) => ConversionResult::Lossy(ConversionChange::UnsignedToSigned(from.as_()))
                    }
                }
            }
        )?
        $(
            impl ConvertableFrom<$from_signed> for $into{
                fn convert_from(from: $from_signed) -> ConversionResult<$into>{
                    match from.try_into(){
                        Ok(ok) => ConversionResult::Changed(ConversionChange::SignedToUnsigned(ok)),
                        Err(_) => ConversionResult::Lossy(ConversionChange::SignedToUnsigned(from.as_()))
                    }
                }
            }
        )?
        $($(
            impl ConvertableFrom<$widening> for $into{
                fn convert_from(from: $widening) -> ConversionResult<$into>{
                    ConversionResult::Changed(ConversionChange::Widening(from.as_()))
                }
            }
        )*)?
        $($(
            impl ConvertableFrom<$narrowing> for $into{
                fn convert_from(from: $narrowing) -> ConversionResult<$into>{
                    ConversionResult::Lossy(ConversionChange::Narrowing(from.as_()))
                }
            }
        )*)?
        $($(
            impl ConvertableFrom<$changed> for $into{
                fn convert_from(from: $changed) -> ConversionResult<$into>{
                    ConversionResult::Changed(ConversionChange::Changed(from.as_()))
                }
            }
        )*)?

        $(
            impl ConvertableFrom<$fnarrow> for $into{
                fn convert_from(from: $fnarrow) -> ConversionResult<$into>{
                    ConversionResult::Lossy(ConversionChange::Narrowing(from.as_()))
                }
            }
        )?
        $(
            impl ConvertableFrom<$fwide> for $into{
                fn convert_from(from: $fwide) -> ConversionResult<$into>{
                    ConversionResult::Changed(ConversionChange::Widening(from.as_()))
                }
            }
        )?
        $($(
            impl ConvertableFrom<$try_narrowing> for $into{
                fn convert_from(from: $try_narrowing) -> ConversionResult<$into>{
                    match from.try_into(){
                        Ok(ok) => ConversionResult::Changed(ConversionChange::Narrowing(ok)),
                        Err(_) => ConversionResult::Lossy(ConversionChange::Narrowing(from.as_()))
                    }
                }
            }
        )*)?
        $($(
            impl ConvertableFrom<$try_widening> for $into{
                fn convert_from(from: $try_widening) -> ConversionResult<$into>{
                    match from.try_into(){
                        Ok(ok) => ConversionResult::Changed(ConversionChange::Widening(ok)),
                        Err(_) => ConversionResult::Lossy(ConversionChange::Widening(from.as_()))
                    }
                }
            }
        )*)?
        $($(
            impl ConvertableFrom<$f2i> for $into{
                fn convert_from(from: $f2i) -> ConversionResult<$into>{
                    ConversionResult::Lossy(ConversionChange::FloatToInt(from.as_()))
                }
            }
        )*)?
        $($(
            impl ConvertableFrom<$i2f> for $into{
                fn convert_from(from: $i2f) -> ConversionResult<$into>{
                    ConversionResult::Lossy(ConversionChange::IntToFloat(from.as_()))
                }
            }
        )*)?
        $($(
            impl ConvertableFrom<$inconvertable> for $into{
                fn convert_from(_: $inconvertable) -> ConversionResult<$into>{
                    ConversionResult::Inconvertable
                }
            }
        )*)?
        $($(
            impl<'a> ConvertableFrom<$asmstr> for $into{
                fn convert_from(_: $asmstr) -> ConversionResult<$into>{
                    ConversionResult::Inconvertable
                }
            }
        )*)?
    };
}

implicit_convert_into!(
    into: bool,
    inconvertable: [i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, f32, f64, char],
    asmstr: [AsmStr<'a>]
);

implicit_convert_into!(
    into: char,
    inconvertable: [i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, f32, f64, bool],
    asmstr: [AsmStr<'a>]
);

implicit_convert_into!(
    into: AsmStr<'a>,
    asmstr: [i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, f32, f64, char, bool]
);

implicit_convert_into!(
    into: u8,
    from_signed: i8,
    try_narrowing: [ i16, i32, i64, i128, u16, u32, u64, u128],
    try_widening: [],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: u16,
    from_signed: i16,
    try_narrowing: [i32, i64, i128, u32, u64, u128],
    try_widening: [u8, i8],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: u32,
    from_signed: i32,
    try_narrowing: [i64, i128, u64, u128],
    try_widening: [u8, i8, i16, u16],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: u64,
    from_signed: i64,
    try_narrowing: [i128, u128],
    try_widening: [u8, i8, i16, u16, i32, u32],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: u128,
    from_signed: i128,
    try_narrowing: [],
    try_widening: [u8, i8, i16, u16, i32, u32, i64, u64],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: i8,
    from_unsigned: u8,
    try_narrowing: [ i16, i32, i64, i128, u16, u32, u64, u128],
    try_widening: [],
    narrowing: [char],
    changed: [bool],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: i16,
    from_unsigned: u16,
    try_narrowing: [i32, i64, i128, u32, u64, u128],
    try_widening: [u8, i8],
    narrowing: [char],
    changed: [bool],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: i32,
    from_unsigned: u32,
    try_narrowing: [ i64, i128, u64, u128],
    try_widening: [u8, i8, i16, u16],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: i64,
    from_unsigned: u64,
    try_narrowing: [i128, u128],
    try_widening: [u8, i8, i16, u16, i32, u32],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: i128,
    from_unsigned: u128,
    try_narrowing: [],
    try_widening: [u8, i8, i16, u16, i32, u32, i64, u64],
    changed: [bool, char],
    i2f: [f32, f64],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: f32,
    fnarrow: f64,
    f2i: [u8, i8, i16, u16, i32, u32, i64, i128, u64, u128],
    inconvertable: [char, bool],
    asmstr: [AsmStr<'a>]
);
implicit_convert_into!(
    into: f64,
    fwide: f32,
    f2i: [u8, i8, i16, u16, i32, u32, i64, i128, u64, u128],
    inconvertable: [char, bool],
    asmstr: [AsmStr<'a>]
);

macro_rules! constant_conversion_functions {
    ($(($ty:ty, $ident:ident, $convert:ident, $checked:ident, $cfg:ident, $checked_with:ident)),* $(,)?) => {
        $(
            pub fn $convert(self) -> ConversionResult<$ty> {
                match self {
                    Constant::$ident(v) => v.convert_into(),
                    other => other.convert_into().changed()
                }
            }

            pub fn $checked(
                self,
                node: NodeRef<'a>,
                ctx: &mut Context<'a>,
            ) -> Option<$ty> {
                self.$checked_with(node, ctx, ctx.config().$cfg)
            }

            pub fn $checked_with(
                self,
                node: NodeRef<'a>,
                ctx: &mut Context<'a>,
                cfg: CastConfig,
            ) -> Option<$ty> {
                self.$convert().implicit_error_message(ValueType::$ident, self.get_type(), node, ctx, cfg)
            }
        )*
    };
}

use crate::config::*;
use crate::context::*;
impl<'a, A: AssemblyLanguage<'a>> Constant<'a, A> {
    constant_conversion_functions!(
        (
            A::Iptr,
            Iptr,
            convert_into_iptr,
            checked_cast_iptr,
            implicit_casts_ints_ptr,
            checked_cast_iptr_with
        ),
        (
            A::Isize,
            Isize,
            convert_into_isize,
            checked_cast_isize,
            implicit_casts_ints_size,
            checked_cast_isize_with
        ),
        (
            A::Uptr,
            Uptr,
            convert_into_uptr,
            checked_cast_uptr,
            implicit_casts_ints_ptr,
            checked_cast_uptr_with
        ),
        (
            A::Usize,
            Usize,
            convert_into_usize,
            checked_cast_usize,
            implicit_casts_ints_size,
            checked_cast_usize_with
        ),
        (
            u8,
            U8,
            convert_into_u8,
            checked_cast_u8,
            implicit_casts_ints,
            checked_cast_u8_with
        ),
        (
            u16,
            U16,
            convert_into_u16,
            checked_cast_u16,
            implicit_casts_ints,
            checked_cast_u16_with
        ),
        (
            u32,
            U32,
            convert_into_u32,
            checked_cast_u32,
            implicit_casts_ints,
            checked_cast_u32_with
        ),
        (
            u64,
            U64,
            convert_into_u64,
            checked_cast_u64,
            implicit_casts_ints,
            checked_cast_u64_with
        ),
        (
            u128,
            U128,
            convert_into_u128,
            checked_cast_u128,
            implicit_casts_ints,
            checked_cast_u128_with
        ),
        (
            i8,
            I8,
            convert_into_i8,
            checked_cast_i8,
            implicit_casts_ints,
            checked_cast_i8_with
        ),
        (
            i16,
            I16,
            convert_into_i16,
            checked_cast_i16,
            implicit_casts_ints,
            checked_cast_i16_with
        ),
        (
            i32,
            I32,
            convert_into_i32,
            checked_cast_i32,
            implicit_casts_ints,
            checked_cast_i32_with
        ),
        (
            i64,
            I64,
            convert_into_i64,
            checked_cast_i64,
            implicit_casts_ints,
            checked_cast_i64_with
        ),
        (
            i128,
            I128,
            convert_into_i128,
            checked_cast_i128,
            implicit_casts_ints,
            checked_cast_i128_with
        ),
        (
            f32,
            F32,
            convert_into_f32,
            checked_cast_f32,
            implicit_casts_ints,
            checked_cast_f32_with
        ),
        (
            f64,
            F64,
            convert_into_f64,
            checked_cast_f64,
            implicit_casts_ints,
            checked_cast_f64_with
        ),
        (
            bool,
            Bool,
            convert_into_bool,
            checked_cast_bool,
            implicit_casts_non_numeric,
            checked_cast_bool_with
        ),
        (
            char,
            Char,
            convert_into_char,
            checked_cast_char,
            implicit_casts_non_numeric,
            checked_cast_char_with
        ),
        (
            AsmStr<'a>,
            Str,
            convert_into_str,
            checked_cast_str,
            implicit_casts_non_numeric,
            checked_cast_str_with
        ),
    );
}

impl<'a, A: AssemblyLanguage<'a>, T> ConvertableFrom<Constant<'a, A>> for T
where
    u8: ConvertableInto<T>,
    u16: ConvertableInto<T>,
    u32: ConvertableInto<T>,
    u64: ConvertableInto<T>,
    u128: ConvertableInto<T>,
    A::Uptr: ConvertableInto<T>,
    A::Usize: ConvertableInto<T>,
    i8: ConvertableInto<T>,
    i16: ConvertableInto<T>,
    i32: ConvertableInto<T>,
    i64: ConvertableInto<T>,
    i128: ConvertableInto<T>,
    A::Iptr: ConvertableInto<T>,
    A::Isize: ConvertableInto<T>,
    f32: ConvertableInto<T>,
    f64: ConvertableInto<T>,
    bool: ConvertableInto<T>,
    char: ConvertableInto<T>,
    AsmStr<'a>: ConvertableInto<T>,
{
    fn convert_from(from: Constant<'a, A>) -> ConversionResult<Self> {
        match from {
            Constant::I8(v) => v.convert_into(),
            Constant::I16(v) => v.convert_into(),
            Constant::I32(v) => v.convert_into(),
            Constant::I64(v) => v.convert_into(),
            Constant::I128(v) => v.convert_into(),
            Constant::Isize(v) => v.convert_into(),
            Constant::Iptr(v) => v.convert_into(),
            Constant::U8(v) => v.convert_into(),
            Constant::U16(v) => v.convert_into(),
            Constant::U32(v) => v.convert_into(),
            Constant::U64(v) => v.convert_into(),
            Constant::U128(v) => v.convert_into(),
            Constant::Usize(v) => v.convert_into(),
            Constant::Uptr(v) => v.convert_into(),
            Constant::F32(v) => v.convert_into(),
            Constant::F64(v) => v.convert_into(),
            Constant::Str(v) => v.convert_into(),
            Constant::Char(v) => v.convert_into(),
            Constant::Bool(v) => v.convert_into(),
        }
    }
}

impl<T> ConvertableFrom<T> for T {
    fn convert_from(from: T) -> ConversionResult<Self> {
        ConversionResult::Identity(from)
    }
}
pub trait ConvertableFrom<T>: Sized {
    fn convert_from(from: T) -> ConversionResult<Self>;
}

pub trait ConvertableInto<T> {
    fn convert_into(self) -> ConversionResult<T>;
}

impl<F, T: ConvertableFrom<F>> ConvertableInto<T> for F {
    fn convert_into(self) -> ConversionResult<T> {
        T::convert_from(self)
    }
}

pub enum ConversionChange<T> {
    Identity(T),
    SignedToUnsigned(T),
    UnsignedToSigned(T),
    Narrowing(T),
    Widening(T),
    FloatToInt(T),
    IntToFloat(T),
    Changed(T),
}

impl<T> ConversionChange<T> {
    pub fn value(self) -> T {
        match self {
            ConversionChange::Identity(v) => v,
            ConversionChange::Changed(v) => v,
            ConversionChange::Narrowing(v) => v,
            ConversionChange::SignedToUnsigned(v) => v,
            ConversionChange::UnsignedToSigned(v) => v,
            ConversionChange::Widening(v) => v,
            ConversionChange::FloatToInt(v) => v,
            ConversionChange::IntToFloat(v) => v,
        }
    }
}

pub enum ConversionResult<T> {
    Identity(T),
    Changed(ConversionChange<T>),
    Lossy(ConversionChange<T>),
    Inconvertable,
}

impl<T> ConversionResult<T> {
    pub fn as_option(self) -> Option<T> {
        match self {
            Self::Identity(v) => Some(v),
            Self::Changed(v) => Some(v.value()),
            Self::Lossy(v) => Some(v.value()),
            Self::Inconvertable => None,
        }
    }

    pub fn changed(self) -> Self {
        if let Self::Identity(v) = self {
            Self::Changed(ConversionChange::Identity(v))
        } else {
            self
        }
    }

    pub fn implicit_error_message<'a, A: AssemblyLanguage<'a>>(
        self,
        desired_ty: ValueType<'a, A>,
        actual_ty: ValueType<'a, A>,
        node: NodeRef<'a>,
        ctx: &mut Context<'a>,
        cfg: CastConfig,
    ) -> Option<T> {
        macro_rules! log {
            ($cfg:expr, $value:expr, $msg:expr) => {
                if $cfg.is_none() {
                    Some($value)
                } else {
                    let kind = if $cfg.is_error() {
                        LogKind::Error
                    } else {
                        LogKind::Warning
                    };
                    ctx.report(LogEntry::new().add(node, kind, $msg).hint_locless(format!(
                        "consider adding explicit cast '{}({}{}{}) as {}{}'",
                        crate::logs::GREEN,
                        crate::logs::RESET,
                        node.src_slice(),
                        crate::logs::GREEN,
                        desired_ty,
                        crate::logs::RESET
                    )));
                    $cfg.is_warning().then_some($value)
                }
            };
        }
        match self {
            ConversionResult::Identity(v) => Some(v),
            ConversionResult::Lossy(c) if cfg.lossy != LogOn::None => match cfg.lossy {
                LogOn::Error | LogOn::Warning => {
                    log!(
                        cfg.lossy,
                        c.value(),
                        format!(
                            "conversion between '{}' and '{}' is lossy",
                            actual_ty, desired_ty
                        )
                    )
                }
                _ => unreachable!(),
            },
            ConversionResult::Lossy(c) | ConversionResult::Changed(c) => match c {
                ConversionChange::Identity(v) => log!(
                    cfg.changed_identity,
                    v,
                    format!(
                        "cannot implicitly perform conversion from '{}' to '{}'",
                        actual_ty, desired_ty
                    )
                ),
                ConversionChange::SignedToUnsigned(v) => {
                    log!(cfg.sign, v, "cannot implicitly change sign of value")
                }
                ConversionChange::UnsignedToSigned(v) => {
                    log!(cfg.sign, v, "cannot implicitly change sign of value")
                }
                ConversionChange::Narrowing(v) => log!(
                    cfg.narrowing,
                    v,
                    format!(
                        "cannot implicitly perform narrowing conversion from '{}' to '{}'",
                        actual_ty, desired_ty
                    )
                ),
                ConversionChange::Widening(v) => log!(
                    cfg.widening,
                    v,
                    format!(
                        "cannot implicitly perform widening conversion from '{}' to '{}'",
                        actual_ty, desired_ty
                    )
                ),
                ConversionChange::FloatToInt(v) => log!(
                    cfg.f2i,
                    v,
                    format!(
                        "cannot implicitly perform conversion from '{}' to '{}'",
                        actual_ty, desired_ty
                    )
                ),
                ConversionChange::IntToFloat(v) => log!(
                    cfg.i2f,
                    v,
                    format!(
                        "cannot implicitly perform conversion from '{}' to '{}'",
                        actual_ty, desired_ty
                    )
                ),
                ConversionChange::Changed(v) => log!(
                    cfg.changed,
                    v,
                    format!(
                        "cannot implicitly perform conversion from '{}' to '{}'",
                        actual_ty, desired_ty
                    )
                ),
            },
            ConversionResult::Inconvertable => None,
        }
    }
}

pub trait FromAsPrimitive<F> {
    fn from_as(v: F) -> Self;
}

impl<F: 'static + Copy, T: 'static + Copy> FromAsPrimitive<F> for T
where
    F: AsPrimitive<T>,
{
    fn from_as(v: F) -> T {
        v.as_()
    }
}

pub trait AsmNum<'a, A: AssemblyLanguage<'a>>:
    PrimInt
    + FromPrimitive
    + Default
    + NumCast
    + ToPrimitive
    + ToBytes
    + WrappingAdd
    + WrappingSub
    + WrappingNeg
    + WrappingMul
    + WrappingShl
    + WrappingShr
    + std::fmt::Display
    + std::fmt::Debug
    + Num<FromStrRadixErr = std::num::ParseIntError>
    + AsPrimitive<u8>
    + AsPrimitive<u16>
    + AsPrimitive<u32>
    + AsPrimitive<u64>
    + AsPrimitive<u128>
    + AsPrimitive<i8>
    + AsPrimitive<i16>
    + AsPrimitive<i32>
    + AsPrimitive<i64>
    + AsPrimitive<i128>
    + AsPrimitive<f32>
    + AsPrimitive<f64>
    + AsPrimitive<Self>
    + AsPrimitive<A::Uptr>
    + AsPrimitive<A::Usize>
    + AsPrimitive<A::Iptr>
    + AsPrimitive<A::Isize>
    + FromAsPrimitive<u8>
    + FromAsPrimitive<u16>
    + FromAsPrimitive<u32>
    + FromAsPrimitive<u64>
    + FromAsPrimitive<u128>
    + FromAsPrimitive<i8>
    + FromAsPrimitive<i16>
    + FromAsPrimitive<i32>
    + FromAsPrimitive<i64>
    + FromAsPrimitive<i128>
    + FromAsPrimitive<f32>
    + FromAsPrimitive<f64>
    + FromAsPrimitive<bool>
    + FromAsPrimitive<char>
    + FromAsPrimitive<Self>
    + FromAsPrimitive<A::Uptr>
    + FromAsPrimitive<A::Usize>
    + FromAsPrimitive<A::Iptr>
    + FromAsPrimitive<A::Isize>
    + ConvertableFrom<u8>
    + ConvertableFrom<u16>
    + ConvertableFrom<u32>
    + ConvertableFrom<u64>
    + ConvertableFrom<u128>
    + ConvertableFrom<i8>
    + ConvertableFrom<i16>
    + ConvertableFrom<i32>
    + ConvertableFrom<i64>
    + ConvertableFrom<i128>
    + ConvertableFrom<f32>
    + ConvertableFrom<f64>
    + ConvertableFrom<char>
    + ConvertableFrom<bool>
    + ConvertableFrom<AsmStr<'a>>
    + ConvertableFrom<Self>
    + ConvertableFrom<A::Uptr>
    + ConvertableFrom<A::Usize>
    + ConvertableFrom<A::Iptr>
    + ConvertableFrom<A::Isize>
    + ConvertableInto<u8>
    + ConvertableInto<u16>
    + ConvertableInto<u32>
    + ConvertableInto<u64>
    + ConvertableInto<u128>
    + ConvertableInto<i8>
    + ConvertableInto<i16>
    + ConvertableInto<i32>
    + ConvertableInto<i64>
    + ConvertableInto<i128>
    + ConvertableInto<f32>
    + ConvertableInto<f64>
    + ConvertableInto<bool>
    + ConvertableInto<char>
    + ConvertableInto<AsmStr<'a>>
    + ConvertableInto<Self>
    + ConvertableInto<A::Uptr>
    + ConvertableInto<A::Usize>
    + ConvertableInto<A::Iptr>
    + ConvertableInto<A::Isize>
{
    const BITS: usize;
    const SIGNED: bool;
}

macro_rules! prims {
    ($($ident:ident),* $(,)?) => {$(
        impl<'a, A: AssemblyLanguage<'a>> AsmNum<'a, A> for $ident
        where
        $ident: ConvertableFrom<A::Iptr> + ConvertableFrom<A::Isize> + ConvertableFrom<A::Uptr> + ConvertableFrom<A::Usize>
        + ConvertableInto<A::Iptr> + ConvertableInto<A::Isize> + ConvertableInto<A::Uptr> + ConvertableInto<A::Usize>,
                $ident: FromAsPrimitive<A::Iptr> + FromAsPrimitive<A::Isize> + FromAsPrimitive<A::Uptr> + FromAsPrimitive<A::Usize>
        + AsPrimitive<A::Iptr> + AsPrimitive<A::Isize> + AsPrimitive<A::Uptr> + AsPrimitive<A::Usize>
        {
                const BITS: usize = std::mem::size_of::<$ident>()*8;
                const SIGNED: bool = stringify!($ident).as_bytes()[0] == b'i';
        }
    )*};
}

prims!(i8, i16, i32, i64, i128, u8, u16, u32, u64, u128);
