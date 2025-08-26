use std::{
    fmt::{Display, Formatter},
    io::Write,
};

use crate::context::Context;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum StringKind {
    #[default]
    Regular,
    Byte,
    CStr,
}

impl StringKind {
    pub fn prefix(self) -> &'static str {
        match self {
            Self::Regular => "",
            Self::Byte => "b",
            Self::CStr => "c",
        }
    }
}

#[derive(Clone, Copy)]
pub enum AsmStr<'a> {
    Str(&'a str),
    ByteStr(&'a [u8]),
    CStr(&'a [u8]),
}

impl<'a> PartialEq<Self> for AsmStr<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::ByteStr(l0), Self::ByteStr(r0)) => l0 == r0,
            (Self::CStr(l0), Self::CStr(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<'a> AsmStr<'a> {
    pub fn len(&self) -> usize {
        match self {
            AsmStr::Str(str) => str.len(),
            AsmStr::ByteStr(str) => str.len(),
            AsmStr::CStr(str) => str.len() + 1,
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_bytes(&self) -> &'a [u8] {
        match self {
            AsmStr::Str(str) => str.as_bytes(),
            AsmStr::ByteStr(str) => str,
            AsmStr::CStr(str) => str,
        }
    }

    pub fn kind(&self) -> StringKind {
        match self {
            Self::Str(_) => StringKind::Regular,
            Self::ByteStr(_) => StringKind::Byte,
            Self::CStr(_) => StringKind::CStr,
        }
    }

    pub fn to_owned(&self) -> AsmString {
        match *self {
            AsmStr::Str(str) => AsmString::String(str.to_owned()),
            AsmStr::ByteStr(str) => AsmString::ByteString(str.to_owned()),
            AsmStr::CStr(str) => AsmString::CString(str.to_owned()),
        }
    }

    pub fn split(&self, char: char) -> AsmStrSplit<'a> {
        match self {
            AsmStr::Str(str) => AsmStrSplit::Reg(str.split(char)),
            AsmStr::ByteStr(items) => AsmStrSplit::Byte(items, char),
            AsmStr::CStr(items) => AsmStrSplit::CStr(items, char),
        }
    }
}

pub enum AsmStrSplit<'a> {
    Reg(std::str::Split<'a, char>),
    Byte(&'a [u8], char),
    CStr(&'a [u8], char),
    Done,
}

impl<'a> Iterator for AsmStrSplit<'a> {
    type Item = AsmStr<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! next_bytes {
            ($self:ident, $str:ident, $char:ident, $kind:ident) => {
                if !$char.is_ascii() {
                    let full = *$str;
                    *$self = Self::Done;
                    AsmStr::$kind(full)
                } else {
                    let b = *$char as u8;
                    if let Some(pos) = $str.iter().position(|c| *c == b) {
                        let first = &$str[..pos];
                        *$str = &$str[pos + 1..];
                        AsmStr::$kind(first)
                    } else {
                        let last = *$str;
                        *$self = Self::Done;
                        AsmStr::$kind(last)
                    }
                }
            };
        }
        Some(match self {
            AsmStrSplit::Reg(split) => AsmStr::Str(split.next()?),
            AsmStrSplit::Byte(str, char) => next_bytes!(self, str, char, ByteStr),
            AsmStrSplit::CStr(str, char) => next_bytes!(self, str, char, CStr),
            AsmStrSplit::Done => None?,
        })
    }
}

#[test]
fn asm_str_split() {
    use AsmStr::ByteStr as B;
    use AsmStr::CStr as C;
    use AsmStr::Str as S;

    assert_eq!(
        B(b"|ss|ss||").split('|').collect::<Vec<_>>(),
        [B(b""), B(b"ss"), B(b"ss"), B(b""), B(b"")]
    );

    assert_eq!(
        C(b"|ss|ss||").split('|').collect::<Vec<_>>(),
        [C(b""), C(b"ss"), C(b"ss"), C(b""), C(b"")]
    );

    assert_eq!(
        S("|ss|ss||").split('|').collect::<Vec<_>>(),
        [S(""), S("ss"), S("ss"), S(""), S("")]
    );

    assert_eq!(
        B(b"ss||ss").split('|').collect::<Vec<_>>(),
        [B(b"ss"), B(b""), B(b"ss")]
    );

    assert_eq!(
        C(b"ss||ss").split('|').collect::<Vec<_>>(),
        [C(b"ss"), C(b""), C(b"ss")]
    );

    assert_eq!(
        S("ss||ss").split('|').collect::<Vec<_>>(),
        [S("ss"), S(""), S("ss")]
    );
}

#[test]
fn meow() {
    let meow = [1, 2, 3, 4, 5, /*6,*/ 7];

    let mut t = 0;

    for n in 1..=7 {
        t ^= n;
    }

    for b in meow {
        t ^= b;
    }
    println!("{t}")
}

impl<'a> Default for AsmStr<'a> {
    fn default() -> Self {
        Self::Str("")
    }
}

impl<'a> Display for AsmStr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmStr::Str(str) => write!(f, "{str}"),
            AsmStr::ByteStr(str) => write!(f, "{}", str.escape_ascii()),
            AsmStr::CStr(str) => write!(f, "{}", str.escape_ascii()),
        }
    }
}

impl<'a> std::fmt::Debug for AsmStr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmStr::Str(str) => write!(f, "{str:?}"),
            AsmStr::ByteStr(str) => write!(f, "b\"{}\"", str.escape_ascii()),
            AsmStr::CStr(str) => write!(f, "c\"{}\"", str.escape_ascii()),
        }
    }
}

impl<'a> From<&'a str> for AsmStr<'a> {
    fn from(value: &'a str) -> Self {
        Self::Str(value)
    }
}

#[derive(Clone)]
pub enum AsmString {
    String(String),
    ByteString(Vec<u8>),
    CString(Vec<u8>),
}

impl PartialEq<Self> for AsmString {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::ByteString(l0), Self::ByteString(r0)) => l0 == r0,
            (Self::CString(l0), Self::CString(r0)) => l0 == r0,
            _ => false,
        }
    }
}

pub enum WriteStrError {
    CannotWriteByteStrToRegularString,
    CannotWriteCStrToRegularString,
}

impl AsmString {
    pub fn write_asm_str(&mut self, to_write: AsmStr<'_>) -> Result<(), WriteStrError> {
        match self {
            AsmString::String(str) => match to_write {
                AsmStr::Str(to_write) => str.push_str(to_write),
                AsmStr::ByteStr(_) => {
                    return Err(WriteStrError::CannotWriteByteStrToRegularString);
                }
                AsmStr::CStr(_) => return Err(WriteStrError::CannotWriteCStrToRegularString),
            },
            AsmString::ByteString(str) | AsmString::CString(str) => {
                _ = str.write_all(to_write.as_bytes())
            }
        }
        Ok(())
    }

    pub fn alloc_str<'a>(&self, ctx: &mut Context<'a>) -> AsmStr<'a> {
        match self {
            AsmString::String(str) => AsmStr::Str(ctx.alloc_str(str)),
            AsmString::ByteString(str) => AsmStr::ByteStr(ctx.alloc_slice(str)),
            AsmString::CString(str) => AsmStr::CStr(ctx.alloc_slice(str)),
        }
    }

    pub fn as_str(&self) -> AsmStr<'_> {
        match self {
            AsmString::String(str) => AsmStr::Str(str.as_str()),
            AsmString::ByteString(str) => AsmStr::ByteStr(str),
            AsmString::CString(str) => AsmStr::CStr(str),
        }
    }

    pub fn new(kind: StringKind) -> Self {
        match kind {
            StringKind::Regular => AsmString::String(String::new()),
            StringKind::Byte => AsmString::ByteString(Vec::new()),
            StringKind::CStr => AsmString::CString(Vec::new()),
        }
    }

    pub fn kind(&self) -> StringKind {
        match self {
            Self::String(_) => StringKind::Regular,
            Self::ByteString(_) => StringKind::Byte,
            Self::CString(_) => StringKind::CStr,
        }
    }
}

impl std::fmt::Write for AsmString {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        match self {
            AsmString::String(str) => str.write_str(s)?,
            AsmString::ByteString(str) => _ = str.write_all(s.as_bytes()),
            AsmString::CString(str) => _ = str.write_all(s.as_bytes()),
        }
        Ok(())
    }
}
