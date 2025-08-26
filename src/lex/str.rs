pub use crate::expression::StringKind;
use crate::lex::small_str::Sstr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum CharKind {
    #[default]
    Regular,
    Byte,
}

impl CharKind {
    pub fn prefix(self) -> &'static str {
        match self {
            Self::Regular => "",
            Self::Byte => "b",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenString<'a> {
    Unparsed(&'a str, StringKind),
    ParsedReg(&'a str),
    ParsedC(&'a [u8]),
    ParsedByte(&'a [u8]),
}
impl<'a> TokenString<'a> {
    pub fn as_bytes(&self) -> &'a [u8] {
        match self {
            Self::Unparsed(str, _) => str.as_bytes(),
            Self::ParsedReg(str) => str.as_bytes(),
            Self::ParsedC(str) => str,
            Self::ParsedByte(str) => str,
        }
    }

    pub fn kind(&self) -> StringKind {
        match self {
            Self::Unparsed(_, kind) => *kind,
            Self::ParsedReg(_) => StringKind::Regular,
            Self::ParsedC(_) => StringKind::CStr,
            Self::ParsedByte(_) => StringKind::Byte,
        }
    }
}

impl Default for TokenString<'static> {
    fn default() -> Self {
        Self::ParsedReg("")
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenChar<'a> {
    Unparsed(Sstr<'a>, CharKind),
    ParsedReg(char),
    ParsedByte(u8),
}

impl Default for TokenChar<'static> {
    fn default() -> Self {
        Self::ParsedReg('\0')
    }
}

impl<'a> TokenChar<'a> {
    pub fn char(char: char) -> Self {
        Self::ParsedReg(char)
    }

    pub fn byte(byte: u8) -> Self {
        Self::ParsedByte(byte)
    }

    pub fn unparsed_reg(repr: &'a str) -> Self {
        Self::Unparsed(repr.into(), CharKind::Regular)
    }

    pub fn unparsed_byte(repr: &'a str) -> Self {
        Self::Unparsed(repr.into(), CharKind::Byte)
    }

    pub fn kind(&self) -> CharKind {
        match self {
            Self::Unparsed(_, kind) => *kind,
            Self::ParsedReg(_) => CharKind::Regular,
            Self::ParsedByte(_) => CharKind::Byte,
        }
    }
}
