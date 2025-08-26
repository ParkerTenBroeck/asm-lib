use std::convert::Infallible;
use std::ffi::OsStr;
use std::marker::PhantomData;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use crate::assembler::lang::AssemblyLanguage;
use crate::context::{Context, Node, NodeRef};
use crate::expression::{
    ArgumentsTypeHint, AsmStr, Constant, ExpressionEvaluator, NodeVal, Value, ValueType,
};
use crate::lex::Token;
use crate::util::IntoStrDelimable;

macro_rules! integer {
    ($(($ty:ty, $kind:expr, $func:ident, $vt:ident, $opt:ident $(, $pow:ident)? $(,)?)),* $(,)?) => {$(
        #[non_exhaustive]
        pub enum $opt<'a, L: AssemblyLanguage<'a>>{
            Val(Option<$ty>),
            __(Infallible, PhantomData<&'a L>)
        }
        impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $opt<'a, L> {
            type LANG = L;
            const TYPE_REPR: &'static str = $kind;
            const HINT: ValueType<'a, L> = ValueType::$vt;

            fn from_arg(
                context: &mut Context<'a>,
                node: NodeRef<'a>,
                value: Value<'a, L>,
            ) -> Result<Self, Option<String>> {
                match value {
                    Value::Constant(c) => Ok($opt::Val(c.$func(node, context))),
                    _ => Err(None),
                }
            }

            fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
                Self::Val(None)
            }
        }

        $(
            #[non_exhaustive]
            pub enum $pow<'a, L: AssemblyLanguage<'a>>{
                Val(Option<$ty>),
                __(Infallible, PhantomData<&'a L>)
            }
            impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $pow<'a, L> {
                type LANG = L;
                const TYPE_REPR: &'static str = $kind;
                const HINT: ValueType<'a, L> = ValueType::$vt;

                fn from_arg(
                    context: &mut Context<'a>,
                    node: NodeRef<'a>,
                    value: Value<'a, L>,
                ) -> Result<Self, Option<String>> {
                    #[allow(unused)]
                    use num_traits::PrimInt;
                    match value {
                        Value::Constant(c) => {
                            let value: $ty = c.$func(node, context).ok_or(None)?;
                            Ok(Self::Val(if value.count_ones() == 1 {
                                    Some(value)
                                } else {
                                    context.report_error(node, "value is not a power of two");
                                    None
                                }
                            ))
                        }
                        _ => Err(None),
                    }
                }

                fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
                    Self::Val(None)
                }
            }
        )?

    )*};
}

integer!(
    (i8, "i8", checked_cast_i8, I8, I8Arg,),
    (i16, "i16", checked_cast_i16, I16, I16Arg,),
    (i32, "i32", checked_cast_i32, I32, I32Arg,),
    (i64, "i64", checked_cast_i64, I64, I64Arg,),
    (i128, "i128", checked_cast_i128, I128, I128Arg,),
    (L::Isize, "isize", checked_cast_isize, Isize, IsizeArg,),
    (L::Iptr, "iptr", checked_cast_iptr, Iptr, IptrArg,),
    (u8, "u8", checked_cast_u8, U8, U8Arg, U8Pow2Arg),
    (u16, "u16", checked_cast_u16, U16, U16Arg, U16Pow2Arg),
    (u32, "u32", checked_cast_u32, U32, U32Arg, U32Pow2Arg),
    (u64, "u64", checked_cast_u64, U64, U64Arg, U64Pow2Arg),
    (u128, "u128", checked_cast_u128, U128, U128Arg, U128Pow2Arg),
    (
        L::Usize,
        "usize",
        checked_cast_usize,
        Usize,
        UsizeArg,
        UsizePow2Arg
    ),
    (
        L::Uptr,
        "uptr",
        checked_cast_uptr,
        Uptr,
        UptrArg,
        UptrPow2Arg
    ),
    (f32, "f32", checked_cast_f32, F32, F32Arg,),
    (f64, "f64", checked_cast_f64, F64, F64Arg,),
    (char, "char", checked_cast_char, Char, CharArg,),
    (bool, "bool", checked_cast_bool, Bool, BoolArg,),
);

// value!(char, "char", Char, CharArg, bool, "bool", Bool, BoolArg,);

#[non_exhaustive]
pub enum AsmStrArg<'a, L> {
    Val(Option<AsmStr<'a>>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for AsmStrArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(str)) => Ok(Self::Val(Some(str))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

#[non_exhaustive]
pub enum PathArg<'a, L> {
    Val(Option<&'a Path>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for PathArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(str)) => Ok(Self::Val(Some(Path::new(
                OsStr::from_bytes(str.as_bytes()),
            )))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

#[non_exhaustive]
pub enum StrArg<'a, L> {
    Val(Option<&'a str>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for StrArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(AsmStr::Str(str))) => Ok(Self::Val(Some(str))),
            Value::Constant(Constant::Str(AsmStr::ByteStr(_))) => Err(Some(
                "byte strings not permitted must be an ordinary string".into(),
            )),
            Value::Constant(Constant::Str(AsmStr::CStr(_))) => Err(Some(
                "c strings not permitted must be an ordinary string".into(),
            )),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

#[non_exhaustive]
pub enum BStrArg<'a, L> {
    Val(Option<&'a [u8]>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for BStrArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "bstr";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(AsmStr::ByteStr(str))) => Ok(Self::Val(Some(str))),
            Value::Constant(Constant::Str(AsmStr::Str(_))) => Err(Some(
                "ordinary strings not permitted must be an byte string".into(),
            )),
            Value::Constant(Constant::Str(AsmStr::CStr(_))) => Err(Some(
                "c strings not permitted must be an byte string".into(),
            )),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

#[non_exhaustive]
pub enum CStrArg<'a, L> {
    Val(Option<&'a [u8]>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for CStrArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "cstr";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(AsmStr::CStr(str))) => Ok(Self::Val(Some(str))),
            Value::Constant(Constant::Str(AsmStr::Str(_))) => Err(Some(
                "ordinary strings not permitted must be an c string".into(),
            )),
            Value::Constant(Constant::Str(AsmStr::ByteStr(_))) => Err(Some(
                "byte strings not permitted must be an c string".into(),
            )),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

#[non_exhaustive]
pub enum BStrRelaxedArg<'a, L> {
    Val(Option<&'a [u8]>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for BStrRelaxedArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str|bstr|cstr";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(str)) => Ok(Self::Val(Some(str.as_bytes()))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

#[non_exhaustive]
pub enum CStrRelaxedArg<'a, L> {
    Val(Option<&'a [u8]>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for CStrRelaxedArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str|bstr|cstr";
    const HINT: ValueType<'a, L> = ValueType::Str;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::Str(str)) => Ok(Self::Val(Some(str.as_bytes()))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Val(None)
    }
}

pub struct RegArg<'a, L: AssemblyLanguage<'a>>(pub Option<L::Reg>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for RegArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, L> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(reg) => Ok(Self(Some(reg))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self(None)
    }
}

pub struct LabelArg<'a, L: AssemblyLanguage<'a>>(pub Option<L::Label>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for LabelArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "label";
    const HINT: ValueType<'a, L> = ValueType::Label;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Label(reg) => Ok(Self(Some(reg))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self(None)
    }
}

pub struct IndexedArg<'a, L: AssemblyLanguage<'a>>(pub Option<L::Indexed>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for IndexedArg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "indexed";
    const HINT: ValueType<'a, L> = ValueType::Indexed;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Indexed(reg) => Ok(Self(Some(reg))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self(None)
    }
}

impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Value<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "any";
    const HINT: ValueType<'a, L> = ValueType::Any;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeRef<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>> {
        Ok(value)
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        Self::Constant(Constant::I32(0))
    }
}

impl<'a, L: AssemblyLanguage<'a>, T: CoercedArg<'a, LANG = L>> CoercedArg<'a> for Node<'a, T> {
    type LANG = L;
    const TYPE_REPR: &'static str = T::TYPE_REPR;
    const HINT: ValueType<'a, L> = T::HINT;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        T::from_arg(context, node, value).map(|v| Node(v, node))
    }

    fn default(context: &mut Context<'a>, node: NodeRef<'a>) -> Self {
        Node(T::default(context, node), node)
    }
}

pub trait CoercedArg<'a>: Sized {
    type LANG: AssemblyLanguage<'a>;
    const TYPE_REPR: &'static str;
    const HINT: ValueType<'a, Self::LANG>;
    const OPTIONAL: bool = false;
    fn from_arg(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>>;

    fn default(context: &mut Context<'a>, node: NodeRef<'a>) -> Self;
}

impl<'a, T: CoercedArg<'a>> CoercedArg<'a> for Option<T> {
    type LANG = T::LANG;
    const TYPE_REPR: &'static str = T::TYPE_REPR;
    const HINT: ValueType<'a, Self::LANG> = T::HINT;
    const OPTIONAL: bool = true;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>> {
        T::from_arg(context, node, value).map(Some)
    }

    fn default(_: &mut Context<'a>, _: NodeRef<'a>) -> Self {
        None
    }
}

pub trait CoercedArgs<'a, L: AssemblyLanguage<'a>> {
    fn from_args(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        args: Node<'a, Vec<NodeVal<'a, L>>>,
    ) -> Node<'a, Self>
    where
        Self: Sized;

    fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, L>) -> R) -> R;

    fn args(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        fb: NodeRef<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        Self::with_hint(|args| ctx.args(fb, args))
    }

    fn args_delim(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        init: NodeRef<'a>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        Self::with_hint(|args| ctx.args_delim(init, start, end, args))
    }

    fn coerced_args(ctx: &mut ExpressionEvaluator<'a, '_, L>, init: NodeRef<'a>) -> Node<'a, Self>
    where
        Self: Sized,
    {
        let args = Self::args(ctx, init);
        Self::from_args(ctx, args)
    }

    fn coerced_args_delim(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        init: NodeRef<'a>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, Self>
    where
        Self: Sized,
    {
        let args = Self::args_delim(ctx, init, start, end);
        Self::from_args(ctx, args)
    }
}

fn wrong_number_args<'a>(
    context: &mut Context<'a>,
    node: NodeRef<'a>,
    args: Vec<Node<'a, Value<'a, impl AssemblyLanguage<'a>>>>,
    expected: &[&str],
    vargs: bool,
) {
    let vargs = if vargs { "*" } else { "" };
    context.report_error(
        node,
        format!(
            "wrong number of arguments, expected [{}{vargs}] got [{}]",
            expected.iter().delim(", "),
            args.iter().map(|i| i.0.get_type()).delim(", "),
        ),
    );
}

fn coerce_argument<'a, L: AssemblyLanguage<'a>, T: CoercedArg<'a, LANG = L>>(
    context: &mut Context<'a>,
    a: Option<Node<'a, Value<'a, L>>>,
    backup: NodeRef<'a>,
) -> T {
    if let Some(Node(arg, node)) = a {
        T::from_arg(context, node, arg)
            .inspect_err(|err| match err {
                None => context.report_error(
                    node,
                    format!(
                        "incorrect argument, expected [{}{}] got [{}]",
                        T::TYPE_REPR,
                        if T::OPTIONAL { "?" } else { "" },
                        arg.get_type()
                    ),
                ),
                Some(msg) => context.report_error(node, msg),
            })
            .unwrap_or_else(|_| T::default(context, node))
    } else {
        T::default(context, backup)
    }
}

macro_rules! nya {
    (!expand,) => {
    };
    (!expand, $v:ident, $($t:ident,)*) => {
        nya!(!expand, $($t,)*);
        nya!(!impl, $($t,)*);
    };
    ($($t:ident),*$(,)?) => {
        nya!(!expand, $($t,)*);
        nya!(!impl, $($t,)*);
    };
    (!impl, $($t:ident),*$(,)?) => {

        #[allow(unused_parens)]
        impl<'a, $($t: CoercedArg<'a, LANG=AL>,)* AL: AssemblyLanguage<'a>> CoercedArgs<'a, AL> for ($($t),*) {
            fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, AL>) -> R) -> R{
                func(ArgumentsTypeHint::Individual(&[$($t::HINT,)*]))
            }

            fn from_args(
                ctx: &mut ExpressionEvaluator<'a, '_, AL>,
                Node(args, node): Node<'a, Vec<NodeVal<'a, AL>>>,
            ) -> Node<'a, Self> {
                nya!(oargs: $($t,)*);
                #[allow(unused_comparisons)]
                if args.len() < nya!(count_min: $($t,)*) || args.len() > nya!(count: $($t,)*) {
                    wrong_number_args(ctx.context, node, args, &[$(&format!("{}{}", $t::TYPE_REPR, if $t::OPTIONAL {"?"} else {""}),)*], false);
                    Node(($($t::default(ctx.context, node)),*), node)
                }else{
                    #[allow(unused)]
                    let mut iter = args.into_iter();
                    nya!(dargs: ctx, node, iter, $($t,)*);
                    Node(($($t),*), node)
                }
            }
        }

        #[allow(unused_parens)]
        impl<'a, $($t: CoercedArg<'a, LANG=AL>,)* VV: CoercedArg<'a, LANG=AL>, AL: AssemblyLanguage<'a>> CoercedArgs<'a, AL> for ($($t,)* Vec<VV>) {
            fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, AL>) -> R) -> R{
                func(ArgumentsTypeHint::Comb(&[$($t::HINT,)*], VV::HINT))
            }

            fn from_args(
                ctx: &mut ExpressionEvaluator<'a, '_, AL>,
                Node(args, node): Node<'a, Vec<NodeVal<'a, AL>>>,
            ) -> Node<'a, Self> {
                nya!(oargs: $($t,)*);
                #[allow(unused_comparisons)]
                if args.len() < nya!(count_min: $($t,)*) {
                    wrong_number_args(ctx.context, node, args, &[$(&format!("{}{}", $t::TYPE_REPR, if $t::OPTIONAL {"?"} else {""}),)* VV::TYPE_REPR], true);
                    Node(($($t::default(ctx.context, node),)* Vec::new()), node)
                }else{
                    #[allow(unused_mut)]
                    let mut iter = args.into_iter();
                    nya!(dargs: ctx, node, iter, $($t,)*);
                    let vv = iter.map(|a|{coerce_argument(ctx.context, Some(a), node)}).collect();
                    Node(($($t,)* vv), node)
                }
            }
        }
    };
    (oargs: $o:ident, $t:ident, $($r:ident,)*) => {
        const{
            if $o::OPTIONAL{
                assert!($t::OPTIONAL, "Non optional parameter cannot follow an optional parameter")
            }
        }
        nya!(oargs: $t, $($r,)*)
    };
    (oargs: $o:ident, ) => {
    };
    (oargs: ) => {
    };
    (dargs: $ctx:expr, $node:expr, $iter:expr, ) => {
    };
    (dargs: $ctx:expr, $node:expr, $iter:expr, $v:ident, $($t:ident,)*) => {
        #[allow(non_snake_case)]
        let $v = coerce_argument($ctx.context, $iter.next(), $node);
        nya!(dargs: $ctx, $node, $iter, $($t,)*);
    };
    (args: $count:expr, $v:ident, $($t:ident),*) => {

    };
    (count:) => {
        0
    };
    (count: $v:ident,) => {
        1
    };
    (count: $v:ident, $($t:ident),*$(,)?) => {
        1+nya!(count: $($t,)*)
    };

    (count_min:) => {
        0
    };
    (count_min: $v:ident,) => {
        ($v::OPTIONAL as usize)
    };
    (count_min: $v:ident, $($t:ident),*$(,)?) => {
        ($v::OPTIONAL as usize)+nya!(count_min: $($t,)*)
    };
}
nya!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
