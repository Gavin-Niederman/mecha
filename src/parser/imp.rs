use crate::{
    Span, Spanned,
    lexer::{Lexeme, Token},
    parser::Block,
};

use super::{
    Identifier, ParseError, ParseResult, Parser,
    ast::{Expr, ExprType, Statement, StatementType, Terminal},
};

impl Parser<'_> {
    pub(super) fn parse_statement(&mut self) -> ParseResult<Statement> {
        fn parse_semicolon(parser: &mut Parser, statement_span: Span) -> ParseResult<Lexeme> {
            parser
                .consume_lexeme_of_type(&[Token::Semicolon])
                .cloned()
                .ok_or_else(|| ParseError::MissingSemicolon {
                    statement_span: statement_span.clone(),
                    expected_semicolon_at: statement_span.end,
                })
        }

        match self
            .consume_lexeme_of_type(&[Token::Let, Token::Return, Token::Closure])
            .cloned()
        {
            Some(
                let_lexeme @ Lexeme {
                    value: Token::Let, ..
                },
            ) => {
                let ident = self.require_token(Token::Identifier)?;
                let ident = Spanned {
                    span: ident.span.clone(),
                    value: Identifier {
                        ident: self.text[ident.span.clone()].to_string(),
                    },
                };

                self.require_token(Token::Equal)?;

                let value = self.parse_expr()?;
                let semicolon = parse_semicolon(self, let_lexeme.span.start..value.span.end)?;

                Ok(Statement {
                    span: let_lexeme.span.start..semicolon.span.end,
                    value: StatementType::Decleration { ident, value },
                })
            }
            Some(
                return_lexeme @ Lexeme {
                    value: Token::Return,
                    ..
                },
            ) => {
                let expr = self.parse_expr()?;
                let semicolon = parse_semicolon(self, return_lexeme.span.start..expr.span.end)?;

                Ok(Statement {
                    span: return_lexeme.span.start..semicolon.span.end,
                    value: StatementType::Return { expr },
                })
            }
            Some(
                fun_lexeme @ Lexeme {
                    value: Token::Closure,
                    ..
                },
            ) => {
                let ident = self.require_token(Token::Identifier)?;
                let ident = Spanned {
                    span: ident.span.clone(),
                    value: Identifier {
                        ident: self.text[ident.span.clone()].to_string(),
                    },
                };

                let (params, block) = self.parse_closure()?;

                let semicolon = parse_semicolon(self, fun_lexeme.span.start..block.span.end)?;

                Ok(Statement {
                    span: fun_lexeme.span.start..semicolon.span.end,
                    value: StatementType::FunctionDeclaration {
                        ident,
                        params,
                        body: block,
                    },
                })
            }
            Some(_) => unreachable!(),
            None => {
                let expr = self.parse_expr()?;
                let semicolon = parse_semicolon(self, expr.span.clone())?;
                Ok(Statement {
                    span: expr.span.start..semicolon.span.end,
                    value: StatementType::DevaluedExpr { expr },
                })
            }
        }
    }

    fn parse_closure(&mut self) -> ParseResult<(Vec<Spanned<Identifier>>, Spanned<Block>)> {
        let Some(paren_open) = self.consume_lexeme_of_type(&[Token::LeftParen]).cloned() else {
            return Err(ParseError::UnexpectedLexeme {
                unexpected_lexeme: self.lexemes[self.location].clone(),
                possible_tokens: vec![Token::LeftParen],
            });
        };

        let mut params = vec![];
        while let Ok(param) = self.require_token(Token::Identifier) {
            let param = Spanned {
                span: param.span.clone(),
                value: Identifier {
                    ident: self.text[param.span.clone()].to_string(),
                },
            };
            params.push(param);
            if self.consume_lexeme_of_type(&[Token::Comma]).is_none() {
                break;
            }
        }

        self.consume_lexeme_of_type(&[Token::RightParen])
            .ok_or_else(|| ParseError::UnclosedDelimiter {
                delimiter: Token::LeftParen,
                start: paren_open.clone(),
            })?;

        let block = self.parse_block()?;

        Ok((params, block))
    }

    #[inline]
    pub(super) fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_if()
    }

    pub(super) fn parse_if(&mut self) -> ParseResult<Expr> {
        let Ok(if_lex) = self.require_token(Token::If) else {
            return self.parse_disjunction();
        };

        let open_paren = self
            .consume_lexeme_of_type(&[Token::LeftParen])
            .ok_or(ParseError::IfConditionLackingParens {
                if_lexeme: if_lex.clone(),
                expected_paren_at: if_lex.span.end,
            })
            .cloned()?;

        let condition = self.parse_disjunction()?;

        self.consume_lexeme_of_type(&[Token::RightParen])
            .ok_or(ParseError::UnclosedDelimiter {
                delimiter: open_paren.value,
                start: open_paren,
            })
            .cloned()?;

        let body = self.parse_block()?;

        let else_body = if self.require_token(Token::Else).is_ok() {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Expr {
            span: if_lex.span.start
                ..else_body
                    .as_ref()
                    .map(|body| body.span.end)
                    .unwrap_or(body.span.end),
            value: ExprType::If {
                condition: Box::new(condition),
                body,
                else_body,
            },
        })
    }

    pub(super) fn parse_disjunction(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_conjunction()?;

        let Ok(_) = self.require_token(Token::Or) else {
            return Ok(lhs);
        };

        let rhs = self.parse_disjunction()?;

        Ok(Expr {
            span: lhs.span.start..rhs.span.end,
            value: ExprType::Disjunction {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        })
    }
    pub(super) fn parse_conjunction(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_equality()?;

        let Ok(_) = self.require_token(Token::And) else {
            return Ok(lhs);
        };

        let rhs = self.parse_conjunction()?;

        Ok(Expr {
            span: lhs.span.start..rhs.span.end,
            value: ExprType::Conjunction {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        })
    }

    pub(super) fn parse_equality(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_comparison()?;

        let Ok(operator) = self.parse_operator(&[Token::Equality, Token::NotEquality]) else {
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
        let Ok(operator) = self.parse_operator(&[Token::Bang, Token::Minus]) else {
            return self.parse_primary();
        };

        let rhs = self.parse_unary()?;

        Ok(Expr {
            span: operator.span.start..rhs.span.end,
            value: ExprType::Unary {
                operator,
                rhs: Box::new(rhs),
            },
        })
    }

    pub(super) fn parse_primary(&mut self) -> ParseResult<Expr> {
        let location = self.location;

        match self.peek_lexeme().cloned() {
            Some(Lexeme {
                value: Token::LeftParen,
                ..
            }) => self.parse_grouping(),
            Some(Lexeme {
                value: Token::LeftBrace,
                ..
            }) => Ok(self.parse_block()?.into()),
            Some(
                ident @ Lexeme {
                    value: Token::Identifier,
                    ..
                },
            ) => {
                self.advance_lexeme();
                let Ok(paren_open) = self.require_token(Token::LeftParen) else {
                    self.location = location;
                    return self.parse_terminal();
                };

                let mut params = vec![];
                while let Ok(expr) = self.parse_expr() {
                    params.push(expr);
                    if self.consume_lexeme_of_type(&[Token::Comma]).is_none() {
                        break;
                    }
                }

                let paren_close = self
                    .consume_lexeme_of_type(&[Token::RightParen])
                    .ok_or_else(|| ParseError::UnclosedDelimiter {
                        delimiter: Token::LeftParen,
                        start: paren_open.clone(),
                    })?;

                Ok(Expr {
                    span: ident.span.start..paren_close.span.end,
                    value: ExprType::Call {
                        ident: Spanned {
                            span: ident.span.clone(),
                            value: Identifier {
                                ident: self.text[ident.span.clone()].to_string(),
                            },
                        },
                        params,
                    },
                })
            }
            Some(
                fun @ Lexeme {
                    value: Token::Closure,
                    ..
                },
            ) => {
                self.advance_lexeme();
                let (params, block) = self.parse_closure()?;

                Ok(Expr {
                    span: fun.span.start..block.span.end,
                    value: ExprType::Terminal(Terminal::Closure {
                        params,
                        body: block,
                    }),
                })
            }
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
            Token::Nil => Terminal::Nil,
            Token::True => Terminal::Boolean(true),
            Token::False => Terminal::Boolean(false),
            Token::Float => Terminal::Float(self.text[lexeme.span.clone()].parse().unwrap()),
            Token::Integer => Terminal::Integer(self.text[lexeme.span.clone()].parse().unwrap()),
            Token::Identifier => Terminal::Ident(Identifier {
                ident: self.text[lexeme.span.clone()].to_string(),
            }),
            _ => {
                return Err(ParseError::UnexpectedLexeme {
                    unexpected_lexeme: lexeme.clone(),
                    possible_tokens: vec![
                        Token::True,
                        Token::False,
                        Token::Float,
                        Token::Integer,
                        Token::Nil,
                    ],
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

        let expr = self.parse_expr()?;

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

    pub(super) fn parse_block(&mut self) -> ParseResult<Spanned<Block>> {
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
                            let expr = self.parse_expr();
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

                Ok(Spanned {
                    span: open_brace.span.start..close_brace.span.end,
                    value: Block {
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

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{
            ParseError, Parser,
            ast::{Expr, ExprType},
        },
    };

    fn lex_and_parse(input: &str) -> Result<Expr, ParseError> {
        let mut lexer = Lexer::new(input);
        let lexemes = lexer.collect().unwrap();

        let mut parser = Parser::new(&lexemes, input);
        parser.parse_expr()
    }

    #[test]
    fn parse_if() {
        let input = "if (1==1) { 1; 1 }";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::If { .. }));
    }

    #[test]
    fn parse_if_missing_paren() {
        let input = "if 1==1 { 1; 1 }";
        let ast = lex_and_parse(input);
        assert!(matches!(
            ast,
            Err(ParseError::IfConditionLackingParens { .. })
        ));
    }

    #[test]
    fn parse_equality() {
        let input = "1==1";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::Equality { .. }));
        let input = "1!=1";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::Equality { .. }));
    }

    #[test]
    fn parse_comparison() {
        let input = "1>1";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::Comparison { .. }));
        let input = "1>=1";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::Comparison { .. }));
        let input = "1<1";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::Comparison { .. }));
        let input = "1<=1";
        let ast = lex_and_parse(input).unwrap();
        assert!(matches!(ast.value, ExprType::Comparison { .. }));
    }
}
