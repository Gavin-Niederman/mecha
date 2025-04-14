mod ast;
mod imp;

use std::fmt::Debug;

pub use ast::*;
use snafu::Snafu;

use crate::{
    Span, Spanned,
    lexer::{Lexeme, Token},
};

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    text: &'a str,
    lexemes: &'a [Lexeme],
    location: usize,
}
impl<'a> Parser<'a> {
    pub const fn new(lexemes: &'a [Lexeme], text: &'a str) -> Self {
        Self {
            lexemes,
            text,
            location: 0,
        }
    }
    pub fn parse(&mut self) -> ParseResult<Ast> {
        let mut statements = vec![];

        loop {
            self.skip_whitespace_lexemes();
            let location = self.location;
            let statement = self.parse_statement();

            match statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(e) => {
                    let eoi = self.lexemes.get(location);
                    let Some(Lexeme {
                        value: Token::Eoi, ..
                    }) = eoi
                    else {
                        return Err(e);
                    };

                    break;
                }
            }
        }

        Ok(statements)
    }

    #[inline]
    fn skip_whitespace_lexemes(&mut self) {
        while let Some(Lexeme {
            value: Token::WhiteSpace,
            ..
        }) = self.lexemes.get(self.location)
        {
            self.advance_lexeme();
        }
    }

    fn consume_lexeme_of_type(&mut self, allowed_tokens: &[Token]) -> Option<&Lexeme> {
        self.skip_whitespace_lexemes();

        let lexeme = self.lexemes.get(self.location);
        if let Some(lexeme) = lexeme {
            if allowed_tokens.contains(&lexeme.value) {
                self.advance_lexeme();
                Some(lexeme)
            } else {
                None
            }
        } else {
            None
        }
    }
    #[inline]
    fn peek_lexeme(&mut self) -> Option<&Lexeme> {
        self.skip_whitespace_lexemes();
        self.lexemes.get(self.location)
    }
    #[inline]
    #[cfg(test)]
    fn consume_lexeme(&mut self) -> Option<&Lexeme> {
        self.skip_whitespace_lexemes();
        let lexeme = self.lexemes.get(self.location);
        self.advance_lexeme();
        lexeme
    }
    #[inline]
    fn advance_lexeme(&mut self) {
        self.location += 1;
    }

    fn require_token(&mut self, token: Token) -> ParseResult<Lexeme> {
        let lexeme = self.peek_lexeme().cloned();

        match lexeme {
            Some(lexeme @ Lexeme { value, .. }) if value == token => {
                self.location += 1;
                Ok(lexeme)
            }
            Some(lexeme) => Err(ParseError::UnexpectedLexeme {
                unexpected_lexeme: lexeme.clone(),
                possible_tokens: [token].into(),
            }),
            None => Err(ParseError::UnexpectedEndOfInput {
                lexeme: self.lexemes[self.location - 1].clone(),
            }),
        }
    }

    fn parse_operator<O: TryFrom<Token>>(
        &mut self,
        allowed_tokens: &[Token],
    ) -> ParseResult<Spanned<O>>
    where
        O::Error: Debug,
    {
        let lexeme = self.peek_lexeme().cloned();

        match lexeme {
            Some(Lexeme { value, span }) if allowed_tokens.contains(&value) => {
                self.advance_lexeme();
                Ok(Spanned {
                    value: O::try_from(value).unwrap(),
                    span,
                })
            }
            Some(lexeme) => Err(ParseError::UnexpectedLexeme {
                unexpected_lexeme: lexeme.clone(),
                possible_tokens: allowed_tokens.into(),
            }),
            None => Err(ParseError::UnexpectedEndOfInput {
                lexeme: self.lexemes[self.location - 1].clone(),
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Snafu)]
pub enum ParseError {
    Multiple {
        errors: Box<[ParseError]>,
    },

    #[snafu(display(
        "Unexpected lexeme: {unexpected_lexeme:?}. Expected one of: {:?}",
        possible_tokens
    ))]
    UnexpectedLexeme {
        unexpected_lexeme: Lexeme,
        possible_tokens: Box<[Token]>,
    },
    #[snafu(display("Unexpected end of input after lexeme: {lexeme:?}"))]
    UnexpectedEndOfInput {
        lexeme: Spanned<Token>,
    },

    #[snafu(display("Unclosed delimiter: {delimiter:?} starting at lexeme: {start:?}"))]
    UnclosedDelimiter {
        delimiter: Token,
        start: Lexeme,
    },

    #[snafu(display("If: {if_lexeme:?} condition was not wrapped in parenthases."))]
    IfConditionLackingParens {
        if_lexeme: Lexeme,
        expected_paren_at: usize,
    },

    BlockNonTrailingExpr {
        block_start: usize,
        expr_span: Span,
    },

    MissingSemicolon {
        statement_span: Span,
        expected_semicolon_at: usize,
    },
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexeme, Token};

    use super::Parser;

    #[test]
    fn ignored_whitespace_lexeme_consumption() {
        let text = "5  +";
        let lexemes = &[
            Lexeme {
                value: Token::Integer,
                span: 0..1,
            },
            Lexeme {
                value: Token::WhiteSpace,
                span: 1..2,
            },
            Lexeme {
                value: Token::Plus,
                span: 2..3,
            },
        ];

        let mut parser = Parser::new(lexemes, text);
        assert_eq!(parser.consume_lexeme().unwrap().value, Token::Integer);
        assert_eq!(parser.consume_lexeme().unwrap().value, Token::Plus);
        assert_eq!(parser.consume_lexeme(), None);
    }
}
