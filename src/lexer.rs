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

macro_rules! token_type {
    {
        $(#[$meta:meta])*
        $vis:vis enum $ident:ident {
            $(
                #[text = $textual:expr]
                $variant:ident
            ),* $(,)?
        }
    } => {
        $(#[$meta])*
        $vis enum $ident {
            $(
                $variant
            ),*
        }

        impl $ident {
            fn next_in_priority(&self) -> Option<$ident> {
                __token_type_prio!(@start self $($variant),*)
            }
        }

        impl Display for $ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        $ident::$variant => write!(f, $textual),
                    )*
                }
            }
        }
    };
}

// An absolutely disgusting macro that builds a priority list for the tokens.
// If the token enum contains the following variants:
// A, B, C, D, E
// Then the macro will build a priority list like this:
// A => Some(B), B => Some(C), C => Some(D), D => Some(E), E => None
//
// Maintaining this will be my personal hell
macro_rules! __token_type_prio {
    (@start $self:ident $($item:ident),*) => {
        __token_type_prio!(@build $self [] $($item),*)
    };
    (@build $self:ident [ $($built:ident => $builtsec:expr),* ] $first:ident, $sec:ident, $($rest:ident),*) => {
        __token_type_prio!(@build $self [ $($built => $builtsec,)* $first => Some(Self::$sec) ] $sec, $($rest),*)
    };
    (@build $self:ident [ $($built:ident => $builtsec:expr),* ] $first:ident, $sec:ident) => {
        __token_type_prio!(@build $self [ $($built => $builtsec,)* $first => Some(Self::$sec) ] $sec)
    };
    (@build $self:ident [ $($built:ident => $builtsec:expr),* ] $first:ident) => {
        __token_type_prio!(@build $self [ $($built => $builtsec,)* $first => None ])
    };
    (@build $self:ident [ $($built:ident => $builtsec:expr),* ]) => {
        match $self {
            $(Self::$built => $builtsec,)*
        }
    }
}

token_type! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Token {
        #[text = "WHITESPACE"]
        WhiteSpace,

        #[text = "FLOAT"]
        Float,
        #[text = "INTEGER"]
        Integer,

        #[text = "'true'"]
        True,
        #[text = "'false'"]
        False,
        #[text = "'nil'"]
        Nil,

        #[text = "'('"]
        LeftParen,
        #[text = "')'"]
        RightParen,
        #[text = "'{{'"]
        LeftBrace,
        #[text = "'}}'"]
        RightBrace,

        #[text = "':'"]
        Colon,
        #[text = "';'"]
        Semicolon,

        #[text = "'+'"]
        Plus,
        #[text = "'-'"]
        Minus,
        #[text = "'*'"]
        Star,
        #[text = "'/'"]
        Slash,

        #[text = "'=='"]
        Equal,
        #[text = "'!='"]
        NotEqual,
        #[text = "'<'"]
        LessThan,
        #[text = "'<='"]
        LessThanOrEqual,
        #[text = "'>'"]
        GreaterThan,
        #[text = "'>='"]
        GreaterThanOrEqual,

        #[text = "'!'"]
        Bang,

        #[text = "'if'"]
        If,

        #[text = "'IDENTIFIER'"]
        Identifier,

        #[text = "'EOI'"]
        Eoi,
    }
}

impl Token {
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
            if self.position >= self.input.len() {
                return Ok(Lexeme::new(self.position..self.position, Token::Eoi));
            }
            let text = &self.input[self.position..];

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

    pub fn collect(&mut self) -> Result<Vec<Lexeme>, LexerError> {
        let mut lexemes = Vec::new();
        loop {
            match self.next_lexeme()? {
                lexeme @ Lexeme {
                    value: Token::Eoi, ..
                } => {
                    lexemes.push(lexeme);
                    break;
                }
                lexeme => lexemes.push(lexeme),
            }
        }

        Ok(lexemes)
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
