pub mod escape;
pub mod lexer;
pub mod number;
pub mod small_str;
pub mod str;
pub mod token;

use std::{fmt::Write, path::Path};

pub use lexer::*;
pub use number::*;
pub use token::*;

use crate::{
    context::{Context, NodeInfoRef, Parent, SourceRef},
    expression::AsmString,
    lex::{
        escape::{EscapeParser, EscapeToken},
        str::{TokenChar, TokenString},
    },
    node::NodeRef,
    preprocess::SrcSlice,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    offset: usize,
    line: usize,
    col: usize,
}

impl Position {
    pub fn march(mut self, char: char) -> Self {
        self.offset += char.len_utf8();
        if char == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self
    }

    pub fn march_optional(self, char: Option<char>) -> Self {
        if let Some(char) = char {
            self.march(char)
        } else {
            self
        }
    }
}

fn parse_char_literal<'a>(
    context: &mut Context<'a>,
    Spanned { span, val }: Spanned<TokenChar<'a>>,
    source: SourceRef<'a>,
    parent: Parent<NodeRef<'a>>,
) -> TokenChar<'a> {
    let (str, kind) = match val {
        TokenChar::Unparsed(sstr, kind) => (sstr.as_str(), kind),
        _ => return val,
    };

    let mut escape_parser = EscapeParser::new(str);

    let res = match escape_parser.next() {
        Some(Spanned {
            val: Ok(EscapeToken::Byte(byte)),
            span: esc_span,
        }) => match kind {
            str::CharKind::Regular if byte.is_ascii() => TokenChar::ParsedReg(byte as char),
            str::CharKind::Regular => {
                context.report_error(
                    context.node(NodeInfoRef {
                        span: span.subspan(esc_span),
                        source,
                        parent,
                    }),
                    format!("{}", LexError::OutOfRangeHexEscape),
                );
                TokenChar::ParsedReg(byte as char)
            }
            str::CharKind::Byte => TokenChar::ParsedByte(byte),
        },
        Some(Spanned {
            val: Ok(EscapeToken::Char(char)),
            span: esc_span,
        }) => match kind {
            str::CharKind::Regular => TokenChar::ParsedReg(char),
            str::CharKind::Byte if char.is_ascii() => TokenChar::ParsedByte(char as u8),
            str::CharKind::Byte => {
                context.report_error(
                    context.node(NodeInfoRef {
                        span: span.subspan(esc_span),
                        source,
                        parent,
                    }),
                    format!("{}", LexError::ByteCharLiteralOutOfRange),
                );
                TokenChar::ParsedByte(0)
            }
        },
        Some(Spanned {
            val: Ok(EscapeToken::Str(str)),
            ..
        }) => {
            if str.chars().count() > 1 {
                context.report_error(
                    context.node(NodeInfoRef {
                        span,
                        source,
                        parent,
                    }),
                    format!("{}", LexError::CharLiteralOverflow),
                );
                match kind {
                    str::CharKind::Regular => TokenChar::ParsedReg('\0'),
                    str::CharKind::Byte => TokenChar::ParsedByte(0),
                }
            } else {
                let c = str.chars().next().unwrap_or_default();
                match kind {
                    str::CharKind::Regular => TokenChar::ParsedReg(c),
                    str::CharKind::Byte if c.is_ascii() => TokenChar::ParsedByte(c as u8),
                    str::CharKind::Byte => {
                        context.report_error(
                            context.node(NodeInfoRef {
                                span,
                                source,
                                parent,
                            }),
                            format!("{}", LexError::ByteCharLiteralOutOfRange),
                        );
                        TokenChar::ParsedByte(0)
                    }
                }
            }
        }
        Some(Spanned {
            val: Err(err),
            span: err_span,
        }) => {
            context.report_error(
                context.node(NodeInfoRef {
                    span: span.subspan(err_span),
                    source,
                    parent,
                }),
                format!("{err}"),
            );
            match kind {
                str::CharKind::Regular => TokenChar::ParsedReg('\0'),
                str::CharKind::Byte => TokenChar::ParsedByte(0),
            }
        }
        None => {
            context.report_error(
                context.node(NodeInfoRef {
                    span,
                    source,
                    parent,
                }),
                format!("{}", LexError::EmpyCharLiteral),
            );
            match kind {
                str::CharKind::Regular => TokenChar::ParsedReg('\0'),
                str::CharKind::Byte => TokenChar::ParsedByte(0),
            }
        }
    };

    if escape_parser.next().is_some() {
        context.report_error(
            context.node(NodeInfoRef {
                span,
                source,
                parent,
            }),
            format!("{}", LexError::CharLiteralOverflow),
        );
    }

    res
}

fn parse_string_literal<'a>(
    context: &mut Context<'a>,
    Spanned { span, val }: Spanned<TokenString<'a>>,
    source: SourceRef<'a>,
    parent: Parent<NodeRef<'a>>,
) -> TokenString<'a> {
    let (str, kind) = match val {
        TokenString::Unparsed(str, kind) => (str, kind),
        _ => return val,
    };
    let mut string = AsmString::new(kind);
    for part in EscapeParser::new(str) {
        match part.val {
            Ok(EscapeToken::Str(str)) => _ = string.write_str(str),
            Ok(EscapeToken::Char(char)) => _ = string.write_char(char),
            Ok(EscapeToken::Byte(byte)) => match &mut string {
                AsmString::String(str) if byte.is_ascii() => str.push(byte as char),
                AsmString::String(_) => {
                    context.report_error(
                        context.node(NodeInfoRef {
                            span: span.subspan(part.span),
                            source,
                            parent,
                        }),
                        format!("{}", LexError::OutOfRangeHexEscape),
                    );
                }
                AsmString::ByteString(str) | AsmString::CString(str) => str.push(byte),
            },
            Err(err) => {
                context.report_error(
                    context.node(NodeInfoRef {
                        span: span.subspan(part.span),
                        source,
                        parent,
                    }),
                    format!("{err}"),
                );
            }
        }
    }
    match string.alloc_str(context) {
        crate::expression::AsmStr::Str(str) => TokenString::ParsedReg(str),
        crate::expression::AsmStr::ByteStr(str) => TokenString::ParsedByte(str),
        crate::expression::AsmStr::CStr(str) => TokenString::ParsedC(str),
    }
}

pub fn lex_file<'a>(
    context: &mut Context<'a>,
    path: &'a Path,
    parent: Parent<NodeRef<'a>>,
) -> Option<SrcSlice<'a>> {
    let result = context.get_text(path);
    let src = match result {
        Ok(stc) => stc,
        Err(error) => {
            if let Some(source) = parent.parent() {
                context.report_error(
                    source,
                    format!("failed to load '{}': {error}", path.display()),
                );
            } else {
                context
                    .report_error_locless(format!("failed to load '{}': {error}", path.display()));
            }
            return None;
        }
    };
    let mut tokens = Vec::new();
    for token in Lexer::new(src.contents).include_comments() {
        match token {
            Ok(token) => match token {
                Spanned {
                    span,
                    val: Token::CharLiteral(str),
                } => tokens.push(Spanned::new(
                    Token::CharLiteral(parse_char_literal(
                        context,
                        Spanned::new(str, span.shrink(str.kind().prefix().len() as u32 + 1, 1)),
                        src,
                        parent,
                    )),
                    span,
                )),
                Spanned {
                    span,
                    val: Token::StringLiteral(str),
                } => tokens.push(Spanned::new(
                    Token::StringLiteral(parse_string_literal(
                        context,
                        Spanned::new(str, span.shrink(str.kind().prefix().len() as u32 + 1, 1)),
                        src,
                        parent,
                    )),
                    span,
                )),
                token => tokens.push(token),
            },
            Err(err) => {
                context.report_error(
                    context.node(NodeInfoRef {
                        span: err.span,
                        source: src,
                        parent,
                    }),
                    format!("{}", err.val),
                );
                use crate::lex::LexError as LE;
                use crate::lex::Token as Tk;
                use crate::lex::str::TokenChar as Tc;
                use crate::lex::str::TokenString as Ts;
                let replacement = match err.val {
                    LE::UnknownChar(_) => continue,
                    LE::UnclosedCharLiteral => Tk::CharLiteral(Tc::ParsedReg('\0')),
                    LE::UnclosedMultiLineComment => continue,
                    LE::UnclosedStringLiteral => Tk::StringLiteral(Ts::ParsedReg("")),
                    LE::EmptyExponent => Tk::NumericLiteral(Number::ZERO),
                    LE::NoNumberAfterBasePrefix => Tk::NumericLiteral(Number::ZERO),
                    LE::ByteCharLiteralOutOfRange => Tk::CharLiteral(Tc::ParsedByte(0)),
                    LE::CharLiteralOverflow => Tk::CharLiteral(Tc::ParsedReg('\0')),
                    LE::EmpyCharLiteral => Tk::CharLiteral(Tc::ParsedReg('\0')),
                    LE::NumberParseError(_) => Tk::NumericLiteral(Number::ZERO),
                    LE::UnknownCharLiteralPrefix(_) => Tk::CharLiteral(Tc::ParsedReg('\0')),
                    LE::UnknownStringLiteralPrefix(_) => Tk::StringLiteral(Ts::ParsedReg("")),
                    LE::OutOfRangeHexEscape => Tk::CharLiteral(Tc::ParsedReg('\0')),
                };
                tokens.push(Spanned::new(replacement, err.span));
            }
        }
    }
    Some(SrcSlice::new(context.alloc_slice(tokens.as_slice()), src))
}
