use std::fmt::Display;

use snafu::Snafu;

use crate::Spanned;

macro_rules! keyword {
    ($kw:literal @ $iter:ident) => {
        const LEN: usize = $kw.len();
        const BYTES: &[u8] = $kw.as_bytes();

        for i in 0..LEN {
            if !(Some(BYTES[i] as char) == $iter.next()) {
                return None;
            }
        }

        return Some(LEN);
    };
}
macro_rules! char_token {
    ($ch:literal @ $iter:ident) => {
        let Some($ch) = $iter.next() else {
            return None;
        };

        return Some(1);
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    WhiteSpace,

    Float,
    Integer,

    True,
    False,
    Nil,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Colon,
    Semicolon,

    Plus,
    Minus,
    Star,
    Slash,

    Bang,

    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    If,

    Identifier,

    Eoi,
}
impl Token {
    fn next_in_priority(&self) -> Option<Token> {
        match self {
            Self::WhiteSpace => Some(Self::Float),

            Self::Float => Some(Self::Integer),
            Self::Integer => Some(Self::True),

            Self::True => Some(Self::False),
            Self::False => Some(Self::Nil),
            Self::Nil => Some(Self::LeftParen),

            Self::LeftParen => Some(Self::RightParen),
            Self::RightParen => Some(Self::LeftBrace),
            Self::LeftBrace => Some(Self::RightBrace),
            Self::RightBrace => Some(Self::Colon),

            Self::Colon => Some(Self::Semicolon),
            Self::Semicolon => Some(Self::Plus),

            Self::Plus => Some(Self::Minus),
            Self::Minus => Some(Self::Star),
            Self::Star => Some(Self::Slash),
            Self::Slash => Some(Self::Bang),

            Self::Bang => Some(Self::Equal),

            Self::Equal => Some(Self::NotEqual),
            Self::NotEqual => Some(Self::LessThan),
            Self::LessThan => Some(Self::LessThanOrEqual),
            Self::LessThanOrEqual => Some(Self::GreaterThan),
            Self::GreaterThan => Some(Self::GreaterThanOrEqual),
            Self::GreaterThanOrEqual => Some(Self::If),

            Self::If => Some(Self::Identifier),

            Self::Identifier => Some(Self::Eoi),
            Self::Eoi => None,
        }
    }

    /// Checks whether the given token is satisfied by the given text.
    ///
    /// # Returns
    ///
    /// A result containing the number of characters that the token consumed
    /// on success and an error on failure.
    pub fn satisfied_by(&self, text: &str) -> Option<usize> {
        let mut text_iter = text.chars();
        let mut consumed = 0;

        match self {
            Token::WhiteSpace => {
                for c in text_iter {
                    if c.is_ascii_whitespace() {
                        consumed += 1;
                    } else {
                        break;
                    }
                }

                if consumed > 0 { Some(consumed) } else { None }
            }

            Token::Integer => {
                for c in text_iter {
                    if c.is_ascii_digit() {
                        consumed += 1;
                    } else {
                        break;
                    }
                }

                if consumed > 0 { Some(consumed) } else { None }
            }
            Token::Float => {
                consumed += Token::Integer.satisfied_by(&text[consumed..])?;

                let mut text_iter = text[consumed..].chars();

                let Some('.') = text_iter.next() else {
                    return None;
                };
                consumed += 1;

                consumed += Token::Integer.satisfied_by(&text[consumed..])?;
                Some(consumed)
            }

            Token::True => {
                keyword!("true" @ text_iter);
            }
            Token::False => {
                keyword!("false" @ text_iter);
            }
            Token::Nil => {
                keyword!("nil" @ text_iter);
            }

            Token::LeftParen => {
                char_token!('(' @ text_iter);
            }
            Token::RightParen => {
                char_token!(')' @ text_iter);
            }
            Token::LeftBrace => {
                char_token!('{' @ text_iter);
            }
            Token::RightBrace => {
                char_token!('}' @ text_iter);
            }

            Token::Colon => {
                char_token!(':' @ text_iter);
            }
            Token::Semicolon => {
                char_token!(';' @ text_iter);
            }

            Token::Plus => {
                char_token!('+' @ text_iter);
            }
            Token::Minus => {
                char_token!('-' @ text_iter);
            }
            Token::Star => {
                char_token!('*' @ text_iter);
            }
            Token::Slash => {
                char_token!('/' @ text_iter);
            }

            Token::Bang => {
                char_token!('!' @ text_iter);
            }

            Token::Equal => {
                keyword!("==" @ text_iter);
            }
            Token::NotEqual => {
                keyword!("!=" @ text_iter);
            }
            Token::LessThan => {
                char_token!('<' @ text_iter);
            }
            Token::LessThanOrEqual => {
                keyword!("<=" @ text_iter);
            }
            Token::GreaterThan => {
                char_token!('>' @ text_iter);
            }
            Token::GreaterThanOrEqual => {
                keyword!(">=" @ text_iter);
            }

            Token::If => {
                keyword!("if" @ text_iter);
            }

            Token::Identifier => {
                for c in text_iter {
                    if c.is_ascii_alphabetic() || c == '_' {
                        consumed += 1;
                    } else {
                        break;
                    }
                }

                if consumed > 0 { Some(consumed) } else { None }
            }

            Token::Eoi => {
                if text.is_empty() {
                    Some(0)
                } else {
                    None
                }
            }
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::WhiteSpace => write!(f, "WHITESPACE"),
            Token::Float => write!(f, "FLOAT"),
            Token::Integer => write!(f, "INTEGER"),
            Token::True => write!(f, "'true'"),
            Token::False => write!(f, "'false'"),
            Token::Nil => write!(f, "'nil'"),
            Token::LeftParen => write!(f, "'('"),
            Token::RightParen => write!(f, "')'"),
            Token::LeftBrace => write!(f, "'{{'"),
            Token::RightBrace => write!(f, "'}}'"),
            Token::Colon => write!(f, "':'"),
            Token::Semicolon => write!(f, "';'"),
            Token::Plus => write!(f, "'+'"),
            Token::Minus => write!(f, "'-'"),
            Token::Star => write!(f, "'*'"),
            Token::Slash => write!(f, "'/'"),
            Token::Bang => write!(f, "'!'"),
            Token::Equal => write!(f, "'=='"),
            Token::NotEqual => write!(f, "'!='"),
            Token::LessThan => write!(f, "'<'"),
            Token::LessThanOrEqual => write!(f, "'<='"),
            Token::GreaterThan => write!(f, "'>'"),
            Token::GreaterThanOrEqual => write!(f, "'>='"),
            Token::If => write!(f, "'if'"),
            Token::Identifier => write!(f, "IDENTIFIER"),
            Token::Eoi => write!(f, "EOI"),
        }
    }
}

pub type Lexeme = Spanned<Token>;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input, position: 0 }
    }

    pub fn next_lexeme(&mut self) -> Result<Lexeme, LexerError> {
        let mut matching_token = Token::WhiteSpace;

        loop {
            let text = &self.input[self.position..];
            if text.is_empty() {
                return Ok(Lexeme::new(self.position..self.position, Token::Eoi));
            }

            match matching_token.satisfied_by(text) {
                Some(consumed) => {
                    let span = self.position..self.position + consumed;

                    self.position += consumed;

                    return Ok(Lexeme::new(span, matching_token));
                }
                None => {
                    matching_token = matching_token.next_in_priority().ok_or(LexerError {
                        index: self.position,
                    })?;
                }
            }
        }
    }

    #[inline]
    pub fn reset(&mut self) {
        self.position = 0;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Snafu)]
#[snafu(display("Failed to parse input starting at {index} as any token."))]
pub struct LexerError {
    pub index: usize,
}

#[cfg(test)]
mod tests {
    use test::Bencher;

    use super::Lexer;

    const LEXER_INPUT: &str = "
aaaaaaaaaaaaaaa (4+ 5.2)
    hi
7-0.2/1";

    #[test]
    fn test_lexemes() {
        let mut lexer = Lexer::new(LEXER_INPUT);

        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::WhiteSpace);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Identifier);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::WhiteSpace);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::LeftParen);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Integer);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Plus);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::WhiteSpace);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Float);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::RightParen);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::WhiteSpace);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Identifier);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::WhiteSpace);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Integer);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Minus);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Float);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Slash);
        assert_eq!(lexer.next_lexeme().unwrap().value, super::Token::Integer);
    }

    #[bench]
    fn bench_lex_token(b: &mut Bencher) {
        let mut lexer = Lexer::new(LEXER_INPUT);
        b.iter(|| {
            lexer.next_lexeme().unwrap();
            lexer.reset();
        });
    }
}
