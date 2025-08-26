use crate::lex::str::{TokenChar, TokenString};

use super::Number;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    LPar,
    RPar,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    ShiftLeft,
    ShiftRight,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Comma,

    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,

    NewLine,

    Assignment,

    ModuloEq,
    Percent,
    DivideEq,
    TimesEq,
    MinusEq,
    PlusEq,
    SmallRightArrow,
    BigRightArrow,
    OrEq,
    AndEq,
    XorEq,
    ShiftRightEq,
    ShiftLeftEq,

    PreProcessorTag(&'a str),
    Label(&'a str),
    Ident(&'a str),

    StringLiteral(TokenString<'a>),
    CharLiteral(TokenChar<'a>),
    NumericLiteral(Number<'a>),
    FalseLiteral,
    TrueLiteral,

    SingleLineComment(&'a str),
    MultiLineComment(&'a str),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LPar => write!(f, "'('"),
            Token::RPar => write!(f, "')'"),
            Token::LBrace => write!(f, "'{{'"),
            Token::RBrace => write!(f, "'}}'"),
            Token::LBracket => write!(f, "'['"),
            Token::RBracket => write!(f, "']'"),
            Token::Plus => write!(f, "'+'"),
            Token::Minus => write!(f, "'-'"),
            Token::Star => write!(f, "'*'"),
            Token::Slash => write!(f, "'/'"),
            Token::Ampersand => write!(f, "'&'"),
            Token::BitwiseOr => write!(f, "'|'"),
            Token::BitwiseXor => write!(f, "'^'"),
            Token::BitwiseNot => write!(f, "'~'"),
            Token::ShiftLeft => write!(f, "'<<'"),
            Token::ShiftRight => write!(f, "'>>'"),
            Token::LogicalAnd => write!(f, "'&&'"),
            Token::LogicalOr => write!(f, "'||'"),
            Token::LogicalNot => write!(f, "'!'"),
            Token::Comma => write!(f, "','"),
            Token::LessThan => write!(f, "'<'"),
            Token::LessThanEq => write!(f, "'<='"),
            Token::GreaterThan => write!(f, "'>'"),
            Token::GreaterThanEq => write!(f, "'>='"),
            Token::Equals => write!(f, "'=='"),
            Token::NotEquals => write!(f, "'!='"),
            Token::NewLine => write!(f, "newline"),
            Token::Assignment => write!(f, "'='"),
            Token::ModuloEq => write!(f, "'%='"),
            Token::Percent => write!(f, "'%'"),
            Token::DivideEq => write!(f, "'/='"),
            Token::TimesEq => write!(f, "'*='"),
            Token::MinusEq => write!(f, "'-='"),
            Token::PlusEq => write!(f, "'+='"),
            Token::SmallRightArrow => write!(f, "'->'"),
            Token::BigRightArrow => write!(f, "'=>'"),
            Token::OrEq => write!(f, "'|='"),
            Token::AndEq => write!(f, "'&='"),
            Token::XorEq => write!(f, "'^='"),
            Token::ShiftRightEq => write!(f, "'>>='"),
            Token::ShiftLeftEq => write!(f, "'<<='"),
            Token::PreProcessorTag(tag) if f.alternate() => write!(f, "'{tag}'"),
            Token::PreProcessorTag(_) => write!(f, "preprocessor tag"),
            Token::Label(label) if f.alternate() => write!(f, "'{label}'"),
            Token::Label(_) => write!(f, "label"),
            Token::Ident(ident) if f.alternate() => write!(f, "'{ident}'"),
            Token::Ident(_) => write!(f, "identifier"),
            Token::StringLiteral(str) if f.alternate() => write!(f, "{str:?}"),
            Token::StringLiteral(_) => write!(f, "string literal"),
            Token::NumericLiteral(number) if f.alternate() => write!(f, "{}", number.get_full()),
            Token::NumericLiteral(_) => write!(f, "numeric literal"),
            Token::CharLiteral(char) if f.alternate() => write!(f, "{char:?}"),
            Token::CharLiteral(_) => write!(f, "char literal"),
            Token::FalseLiteral => write!(f, "false"),
            Token::TrueLiteral => write!(f, "true"),
            Token::SingleLineComment(_) => write!(f, "comment"),
            Token::MultiLineComment(_) => write!(f, "comment"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self { val, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            val: f(self.val),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: u32,
    pub col: u32,
    pub offset: u32,
    pub len: u32,
}

impl Span {
    pub const fn empty() -> Span {
        Span {
            line: 0,
            col: 0,
            offset: 0,
            len: 0,
        }
    }

    pub(super) fn start_end(start: super::Position, end: super::Position) -> Self {
        Span {
            line: start.line as u32,
            col: start.col as u32,
            offset: start.offset as u32,
            len: (end.offset - start.offset) as u32,
        }
    }

    pub fn subspan(mut self, sub: Span) -> Self {
        self.offset += sub.offset;
        self.len = sub.len;
        if sub.line == 0 {
            self.col += sub.col;
        } else {
            self.line += sub.line;
            self.col = sub.col;
        }
        self
    }

    pub fn combine(&self, other: Span) -> Span {
        let send = self.len.wrapping_add(self.offset);
        let oend = other.len.wrapping_add(other.offset);
        let offset = self.offset.min(other.offset);
        Span {
            line: self.line.min(other.line),
            col: self.col.min(other.col),
            offset,
            len: if send > oend {
                send - offset
            } else {
                oend - offset
            },
        }
    }

    pub fn shrink(self, left: u32, right: u32) -> Span {
        let len = self.len;
        let left = left.min(len);
        let len = len.saturating_sub(left);
        let right = right.min(len);
        let len = len.saturating_sub(right);

        let offset = self.offset.saturating_add(left);
        let col = self.col.saturating_add(left);

        Span {
            line: self.line,
            col,
            offset,
            len,
        }
    }
}
