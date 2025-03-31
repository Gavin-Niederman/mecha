use crate::{
    Spanned,
    lexer::{Lexeme, Token},
};

use super::{
    ParseError, ParseResult, Parser,
    ast::{Expr, ExprType, Statement, StatementType, Terminal},
};

impl Parser<'_> {
    pub(super) fn parse_statement(&mut self) -> ParseResult<Statement> {
        let expr = self.parse_if()?;
        let semicolon = self
            .consume_lexeme_of_type(&[Token::Semicolon])
            .ok_or_else(|| ParseError::MissingSemicolon {
                statement_span: expr.span.clone(),
                expected_semicolon_at: expr.span.end,
            })?;

        Ok(Statement {
            span: expr.span.start..semicolon.span.end,
            value: StatementType::DevaluedExpr { expr },
        })
    }

    pub(super) fn parse_if(&mut self) -> ParseResult<Expr> {
        let Ok(if_lex) = self.require_token(Token::If) else {
            return self.parse_equality();
        };

        let open_paren = self
            .consume_lexeme_of_type(&[Token::LeftParen])
            .ok_or(ParseError::IfConditionLackingParens {
                if_lexeme: if_lex.clone(),
                expected_paren_at: if_lex.span.end,
            })
            .cloned()?;

        let condition = self.parse_equality()?;

        let close_paren = self
            .consume_lexeme_of_type(&[Token::RightParen])
            .ok_or(ParseError::UnclosedDelimiter {
                delimiter: open_paren.value,
                start: open_paren,
            })
            .cloned()?;

        let body = self.parse_block()?;

        Ok(Expr {
            span: if_lex.span.start..close_paren.span.end,
            value: ExprType::If {
                condition: Box::new(condition),
                body: Box::new(body),
            },
        })
    }

    pub(super) fn parse_equality(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_comparison()?;

        let Ok(operator) = self.parse_operator(&[Token::Equal, Token::NotEqual]) else {
            return Ok(lhs);
        };

        let rhs = self.parse_equality()?;

        Ok(Expr {
            span: lhs.span.start..rhs.span.end,
            value: ExprType::Equality {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            },
        })
    }

    pub(super) fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_term()?;

        let Ok(operator) = self.parse_operator(&[
            Token::GreaterThan,
            Token::GreaterThanOrEqual,
            Token::LessThan,
            Token::LessThanOrEqual,
        ]) else {
            return Ok(lhs);
        };

        let rhs = self.parse_comparison()?;

        Ok(Expr {
            span: lhs.span.start..rhs.span.end,
            value: ExprType::Comparison {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            },
        })
    }

    pub(super) fn parse_term(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_factor()?;

        let Ok(operator) = self.parse_operator(&[Token::Minus, Token::Plus]) else {
            return Ok(lhs);
        };

        let rhs = self.parse_term()?;

        Ok(Expr {
            span: lhs.span.start..rhs.span.end,
            value: ExprType::Term {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            },
        })
    }

    pub(super) fn parse_factor(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_unary()?;

        let Ok(operator) = self.parse_operator(&[Token::Star, Token::Slash]) else {
            return Ok(lhs);
        };

        let rhs = self.parse_factor()?;

        Ok(Expr {
            span: lhs.span.start..rhs.span.end,
            value: ExprType::Factor {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            },
        })
    }

    pub(super) fn parse_unary(&mut self) -> ParseResult<Expr> {
        let lexeme = self.peek_lexeme().cloned();

        match lexeme {
            Some(
                lexeme @ Lexeme {
                    value: Token::Bang | Token::Minus,
                    ..
                },
            ) => {
                // Peeked lexeme used
                self.advance_lexeme();

                let operator = Spanned::new(lexeme.span.clone(), lexeme.value.try_into().unwrap());

                let rhs = self.parse_unary()?;

                Ok(Expr {
                    span: lexeme.span.start..rhs.span.end,
                    value: ExprType::Unary {
                        operator,
                        rhs: Box::new(rhs),
                    },
                })
            }
            Some(_) => self.parse_primary(),
            None => Err(ParseError::UnexpectedEndOfInput {
                lexeme: self.lexemes[self.location - 1].clone(),
            }),
        }
    }

    pub(super) fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.peek_lexeme() {
            Some(Lexeme {
                value: Token::LeftParen,
                ..
            }) => self.parse_grouping(),
            Some(Lexeme {
                value: Token::LeftBrace,
                ..
            }) => self.parse_block(),
            _ => self.parse_terminal(),
        }
    }

    pub(super) fn parse_terminal(&mut self) -> ParseResult<Expr> {
        let Some(lexeme) = self.peek_lexeme().cloned() else {
            return Err(ParseError::UnexpectedEndOfInput {
                lexeme: self.lexemes[self.location - 1].clone(),
            });
        };

        let terminal = match lexeme.value {
            Token::True => Terminal::Boolean(true),
            Token::False => Terminal::Boolean(false),
            Token::Float => Terminal::Float(self.text[lexeme.span.clone()].parse().unwrap()),
            Token::Integer => Terminal::Integer(self.text[lexeme.span.clone()].parse().unwrap()),
            _ => {
                return Err(ParseError::UnexpectedLexeme {
                    unexpected_lexeme: lexeme.clone(),
                    possible_tokens: vec![Token::True, Token::False, Token::Float, Token::Integer],
                });
            }
        };
        // Peeked lexeme used
        self.advance_lexeme();

        Ok(Expr {
            span: lexeme.span.clone(),
            value: ExprType::Terminal(terminal),
        })
    }

    pub(super) fn parse_grouping(&mut self) -> ParseResult<Expr> {
        let Some(paren_open) = self.consume_lexeme_of_type(&[Token::LeftParen]).cloned() else {
            return Err(ParseError::UnexpectedLexeme {
                unexpected_lexeme: self.lexemes[self.location].clone(),
                possible_tokens: vec![Token::LeftParen],
            });
        };

        let expr = self.parse_if()?;

        let paren_close = self
            .consume_lexeme_of_type(&[Token::RightParen])
            .ok_or_else(|| ParseError::UnclosedDelimiter {
                delimiter: Token::LeftParen,
                start: paren_open.clone(),
            })?;

        Ok(Expr {
            span: paren_open.span.start..paren_close.span.end,
            value: expr.value,
        })
    }

    pub(super) fn parse_block(&mut self) -> ParseResult<Expr> {
        let lexeme = self.peek_lexeme().cloned();
        match lexeme {
            Some(
                open_brace @ Lexeme {
                    value: Token::LeftBrace,
                    ..
                },
            ) => {
                // Peeked lexeme used
                self.advance_lexeme();

                enum BlockItem {
                    Statement(Statement),
                    Expr(Expr),
                }

                let mut items = Vec::new();
                while let Some(lexeme) = self.peek_lexeme() {
                    match lexeme.value {
                        Token::RightBrace => {
                            // Block ended
                            break;
                        }
                        _ => {
                            let current_location = self.location;

                            let statement = self.parse_statement();
                            if let Ok(statement) = statement {
                                items.push(BlockItem::Statement(statement));
                                continue;
                            }

                            self.location = current_location;
                            let expr = self.parse_if();
                            if let Ok(expr) = expr {
                                items.push(BlockItem::Expr(expr));
                                continue;
                            }

                            return Err(ParseError::Multiple {
                                errors: vec![statement.unwrap_err(), expr.unwrap_err()],
                            });
                        }
                    }
                }

                let mut trailing_expr = None;

                for (i, item) in items.iter().enumerate() {
                    if i >= items.len() - 1 {
                        if let BlockItem::Expr(expr) = item {
                            trailing_expr = Some(expr.clone());
                        }
                    } else if let BlockItem::Expr(expr) = item {
                        return Err(ParseError::BlockNonTrailingExpr {
                            block_start: open_brace.span.start,
                            expr_span: expr.span.clone(),
                        });
                    }
                }

                let statements = items
                    .into_iter()
                    .filter_map(|item| match item {
                        BlockItem::Statement(statement) => Some(statement),
                        BlockItem::Expr(_) => None,
                    })
                    .collect();

                let close_brace = self
                    .consume_lexeme_of_type(&[Token::RightBrace])
                    .ok_or_else(|| ParseError::UnclosedDelimiter {
                        delimiter: Token::LeftBrace,
                        start: open_brace.clone(),
                    })?;

                Ok(Expr {
                    span: open_brace.span.start..close_brace.span.end,
                    value: ExprType::Block {
                        statements,
                        ret: trailing_expr.map(Box::new),
                    },
                })
            }
            Some(unexpected) => Err(ParseError::UnexpectedLexeme {
                unexpected_lexeme: unexpected.clone(),
                possible_tokens: vec![Token::LeftBrace],
            }),
            None => Err(ParseError::UnexpectedEndOfInput {
                lexeme: self.lexemes[self.location - 1].clone(),
            }),
        }
    }
}
