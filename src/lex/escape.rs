use std::{iter::Peekable, str::Chars};

use crate::lex::{Position, Span, Spanned};

enum State {
    Default,
    Escape,
    EscapeHexStart,
    EscapeHexEnd(char),
    EscapeUnicodeStart,
    EscapeUnicodeData(u32, u8),
    InvalidCharacterInUnicodeEscape,
    EscapeNl,
}

#[derive(Clone, Copy)]
pub enum EscapeToken<'a> {
    Str(&'a str),
    Char(char),
    Byte(u8),
}

#[derive(Clone, Copy, Debug)]
pub enum EscapeError {
    UnfinishedEscape,
    UnknownEscape(char),
    UnfinishedHexEscape,
    InvalidHexCharacter,
    UnclosedUnicodeEscape,
    ExpectedOpeningBrace,
    InvalidUnicodeEscapeCharacter,
    InvalidUnicodeCodepoint(u32),
    UnicodeEscapeTooLong,
    UnicodeEscapeEmpty,
}

impl std::fmt::Display for EscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EscapeError::UnfinishedEscape => write!(f, "unfinished escape"),
            EscapeError::UnknownEscape(c) => write!(f, "unknown escape {c:?}"),
            EscapeError::UnfinishedHexEscape => write!(f, "unfinished hex escape"),
            EscapeError::InvalidHexCharacter => write!(
                f,
                "invalid character in hex escape must be 0..=9|'a'..='f'|'A'..='F'"
            ),
            EscapeError::UnclosedUnicodeEscape => write!(f, "unclosed unicode escape"),
            EscapeError::ExpectedOpeningBrace => write!(f, "expected opening brace"),
            EscapeError::InvalidUnicodeEscapeCharacter => write!(
                f,
                "invalid character in hexunicode escape must be 0..=9|'a'..='f'|'A'..='F'"
            ),
            EscapeError::InvalidUnicodeCodepoint(_) => write!(f, "invalid codepoint"),
            EscapeError::UnicodeEscapeTooLong => write!(
                f,
                "unicode escape sequence too long must be 1-6 digits long"
            ),
            EscapeError::UnicodeEscapeEmpty => write!(
                f,
                "unicode escape sequence too short must be 1-6 digits long"
            ),
        }
    }
}

pub struct EscapeParser<'a> {
    str: &'a str,
    chars: Peekable<Chars<'a>>,
    state: State,

    start: Position,
    current: Position,
}

impl<'a> EscapeParser<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            str,
            chars: str.chars().peekable(),
            state: State::Default,
            start: Position::default(),
            current: Position::default(),
        }
    }

    fn current_str(&self) -> &'a str {
        &self.str[self.start.offset..self.current.offset]
    }
}

impl<'a> Iterator for EscapeParser<'a> {
    type Item = Spanned<Result<EscapeToken<'a>, EscapeError>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut processing;
        let res = loop {
            let c = self.chars.peek().copied();
            processing = self.current.march_optional(c);

            match self.state {
                State::Default => match c {
                    Some('\\') => {
                        self.state = State::Escape;
                        if !self.current_str().is_empty() {
                            break Ok(EscapeToken::Str(self.current_str()));
                        }
                    }
                    None => {
                        if self.current_str().is_empty() {
                            return None;
                        } else {
                            break Ok(EscapeToken::Str(self.current_str()));
                        }
                    }
                    _ => {}
                },
                State::Escape => {
                    self.state = State::Default;
                    match c {
                        Some('\n' | '\r') => self.state = State::EscapeNl,
                        Some('\'') => break Ok(EscapeToken::Char('\'')),
                        Some('"') => break Ok(EscapeToken::Char('"')),
                        Some('0') => break Ok(EscapeToken::Char('\0')),
                        Some('n') => break Ok(EscapeToken::Char('\n')),
                        Some('r') => break Ok(EscapeToken::Char('\r')),
                        Some('\\') => break Ok(EscapeToken::Char('\\')),
                        Some('t') => break Ok(EscapeToken::Char('\t')),
                        Some('x') => self.state = State::EscapeHexStart,
                        Some('u') => self.state = State::EscapeUnicodeStart,
                        Some(c) => break Err(EscapeError::UnknownEscape(c)),
                        None => break Err(EscapeError::UnfinishedEscape),
                    }
                }
                State::EscapeNl => match c {
                    Some('\n' | '\r') => {}
                    _ => {
                        self.state = State::Default;
                        continue;
                    }
                },
                State::EscapeHexStart => match c {
                    Some(c) => self.state = State::EscapeHexEnd(c),
                    None => break Err(EscapeError::UnfinishedHexEscape),
                },
                State::EscapeHexEnd(msb) => match c {
                    Some(lsb) => {
                        self.state = State::Default;
                        if lsb.is_ascii_hexdigit() && msb.is_ascii_hexdigit() {
                            let msb = msb.to_digit(16).unwrap_or_default() as u8;
                            let lsb = lsb.to_digit(16).unwrap_or_default() as u8;
                            break Ok(EscapeToken::Byte((msb << 4) + lsb));
                        } else {
                            break Err(EscapeError::InvalidHexCharacter);
                        }
                    }
                    None => break Err(EscapeError::UnfinishedHexEscape),
                },
                State::EscapeUnicodeStart => match c {
                    Some('{') => self.state = State::EscapeUnicodeData(0u32, 0u8),
                    Some(_) => break Err(EscapeError::ExpectedOpeningBrace),
                    None => break Err(EscapeError::UnclosedUnicodeEscape),
                },
                State::EscapeUnicodeData(code, count) => match c {
                    Some(c) if c.is_ascii_hexdigit() => {
                        let dig = c.to_digit(16).unwrap_or(0);
                        self.state =
                            State::EscapeUnicodeData((code << 4) + dig, count.saturating_add(1))
                    }
                    Some('}') => {
                        self.state = State::Default;
                        if count == 0 {
                            break Err(EscapeError::UnicodeEscapeEmpty);
                        } else if count > 6 {
                            break Err(EscapeError::UnicodeEscapeTooLong);
                        } else if let Some(char) = char::from_u32(code) {
                            break Ok(EscapeToken::Char(char));
                        } else {
                            break Err(EscapeError::InvalidUnicodeCodepoint(code));
                        }
                    }
                    Some(_) => self.state = State::InvalidCharacterInUnicodeEscape,
                    None => break Err(EscapeError::UnclosedUnicodeEscape),
                },
                State::InvalidCharacterInUnicodeEscape => match c {
                    Some('}') => break Err(EscapeError::InvalidUnicodeEscapeCharacter),
                    Some(_) => {}
                    None => break Err(EscapeError::UnclosedUnicodeEscape),
                },
            }
            self.chars.next();
            self.current = processing;
        };
        self.chars.next();
        self.current = processing;
        let start = self.start;
        self.start = self.current;
        if res.is_err() {
            self.state = State::Default;
        }
        Some(Spanned::new(res, Span::start_end(start, self.current)))
    }
}

#[test]
fn escape() {
    for part in EscapeParser::new(r#"meow\\ \' \" \0 \r \n \t \x12 nyaaaaaa \u \u{ffffffff} \"#) {
        match part.val {
            Ok(EscapeToken::Str(str)) => println!("{str:?}"),
            Ok(EscapeToken::Char(str)) => println!("{str:?}"),
            Ok(EscapeToken::Byte(str)) => println!("{str:?}"),
            Err(error) => println!("{error:?}"),
        }
    }
}
