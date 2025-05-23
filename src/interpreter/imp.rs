use crate::{
    Span, Spannable, Spanned,
    interpreter::{
        Scope,
        value::{ValueType, check_values_valid_for_math},
    },
    parser::{
        Block, ComparisonOperator, EqualityOperator, Expr, ExprType, FactorOperator, Identifier,
        Statement, StatementType, TermOperator, Terminal, UnaryOperator,
    },
};

use super::{Interpreter, InterpreterError, value::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOperator {
    Disjunction,
    Conjunction,

    Equal,
    NotEqual,

    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,

    Plus,
    Minus,

    Multiply,
    Divide,
}
impl From<EqualityOperator> for BinaryOperator {
    fn from(op: EqualityOperator) -> Self {
        match op {
            EqualityOperator::Equal => Self::Equal,
            EqualityOperator::NotEqual => Self::NotEqual,
        }
    }
}
impl From<ComparisonOperator> for BinaryOperator {
    fn from(op: ComparisonOperator) -> Self {
        match op {
            ComparisonOperator::GreaterThan => Self::GreaterThan,
            ComparisonOperator::GreaterThanOrEqual => Self::GreaterThanOrEqual,
            ComparisonOperator::LessThan => Self::LessThan,
            ComparisonOperator::LessThanOrEqual => Self::LessThanOrEqual,
        }
    }
}
impl From<TermOperator> for BinaryOperator {
    fn from(op: TermOperator) -> Self {
        match op {
            TermOperator::Plus => Self::Plus,
            TermOperator::Minus => Self::Minus,
        }
    }
}
impl From<FactorOperator> for BinaryOperator {
    fn from(op: FactorOperator) -> Self {
        match op {
            FactorOperator::Multiply => Self::Multiply,
            FactorOperator::Divide => Self::Divide,
        }
    }
}

macro_rules! apply_op_to_value {
    (@num $lhs:ident, $rhs:ident, |$l:ident, $r:ident| $body:expr) => {
        match ($lhs.value, $rhs.value) {
            (Value::Integer($l), Value::Integer($r)) => {
                Value::Integer($body).spanned($lhs.span.start..$rhs.span.start)
            }
            (Value::Float($l), Value::Float($r)) => {
                Value::Float($body).spanned($lhs.span.start..$rhs.span.start)
            }
            _ => unreachable!(),
        }
    };
    (@bool $lhs:ident, $rhs:ident, |$l:ident, $r:ident| $body:expr) => {
        match ($lhs.value, $rhs.value) {
            (Value::Integer($l), Value::Integer($r)) => {
                Value::Boolean($body).spanned($lhs.span.start..$rhs.span.start)
            }
            (Value::Float($l), Value::Float($r)) => {
                Value::Boolean($body).spanned($lhs.span.start..$rhs.span.start)
            }
            _ => unreachable!(),
        }
    };
}

#[derive(Debug, Clone)]
enum Frame {
    EvaluateExpr(Expr),
    EvaluateStatement(Statement),

    ApplyBinop(BinaryOperator),
    ApplyUnaryOp(UnaryOperator),
    Array {
        span: Span,
        num_elements: usize,
    },

    CallStart {
        num_args: usize,
        ident: Spanned<Identifier>,
        span: Span,
    },
    CallEnd {
        original_top_scope: u64,
    },

    ScopeEnd,

    If {
        span: Span,
        body: Spanned<Block>,
        else_body: Option<Spanned<Block>>,
    },
    While {
        condition: Expr,
        body: Spanned<Block>,
    },

    DevalueExpr,
    Declare {
        ident: Spanned<Identifier>,
    },
}

impl Interpreter {
    pub fn visit_statement(&mut self, statement: Statement) -> Result<(), InterpreterError> {
        // Used to step the interpreter
        let mut command_stack = vec![Frame::EvaluateStatement(statement)];
        // Stack of finished values
        let mut value_stack = vec![];
        // Stack of function block ids
        let mut function_stack = vec![];

        while let Some(frame) = command_stack.pop() {
            let super_scope = self.super_scope();

            match frame {
                // Evaluation
                Frame::EvaluateExpr(Expr {
                    span,
                    value: ExprType::Terminal(terminal),
                }) => match terminal {
                    Terminal::Nil => {
                        value_stack.push(Value::Nil.spanned(span));
                    }
                    Terminal::Boolean(boolean) => {
                        value_stack.push(Value::Boolean(boolean).spanned(span));
                    }
                    Terminal::Float(float) => {
                        value_stack.push(Value::Float(float).spanned(span));
                    }
                    Terminal::Integer(int) => {
                        value_stack.push(Value::Integer(int).spanned(span));
                    }
                    Terminal::String(string) => {
                        value_stack.push(Value::String(string).spanned(span));
                    }
                    Terminal::Ident(ident) => {
                        value_stack
                            .push(super_scope.get(&ident.ident, span.clone())?.spanned(span));
                    }
                    Terminal::Closure { params, body } => {
                        value_stack.push(
                            Value::Function {
                                params: params.into_iter().map(|param| param.value.ident).collect(),
                                body: body.value,
                                defined_in_scope: self.top_scope,
                            }
                            .spanned(span),
                        );
                        // This scope depends on itself due to the closure.
                        // This means it can literally never be dropped.
                        // A better solution would remove this cyclic dependency when the closure is not used.
                        self.scopes
                            .get_mut(&self.top_scope)
                            .unwrap()
                            .reference_count += 1;
                    }
                },
                Frame::EvaluateExpr(Expr {
                    span,
                    value: ExprType::Array(items),
                }) => {
                    command_stack.push(Frame::Array { span, num_elements: items.len() });
                    command_stack.extend(
                        items.into_iter().map(Frame::EvaluateExpr),
                    );
                }

                Frame::EvaluateExpr(Expr {
                    span,
                    value: ExprType::Call { ident, params },
                }) => {
                    command_stack.push(Frame::CallStart {
                        span,
                        ident: ident.clone(),
                        num_args: params.len(),
                    });

                    command_stack.push(Frame::EvaluateExpr(Expr {
                        value: ExprType::Terminal(Terminal::Ident(ident.value)),
                        span: ident.span,
                    }));

                    for param in params {
                        command_stack.push(Frame::EvaluateExpr(param));
                    }
                }
                Frame::EvaluateExpr(Expr {
                    span: _span,
                    value: ExprType::Block(block),
                }) => {
                    self.push_scope(Scope::new(Some(self.top_scope)));

                    command_stack.push(Frame::ScopeEnd);

                    if let Some(ret) = block.ret {
                        command_stack.push(Frame::EvaluateExpr(*ret));
                    }
                    command_stack.extend(
                        block
                            .statements
                            .into_iter()
                            .rev()
                            .map(Frame::EvaluateStatement),
                    );
                }
                Frame::ScopeEnd => {
                    self.pop_scope();
                    // self.garbage_collect_scopes();
                }
                Frame::EvaluateExpr(Expr {
                    span,
                    value:
                        ExprType::If {
                            condition,
                            body,
                            else_body,
                        },
                }) => {
                    command_stack.push(Frame::If {
                        span,
                        body,
                        else_body,
                    });

                    command_stack.push(Frame::EvaluateExpr(*condition));
                }

                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Unary { operator, rhs },
                }) => {
                    command_stack.push(Frame::ApplyUnaryOp(operator.value));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }
                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Disjunction { lhs, rhs },
                }) => {
                    command_stack.push(Frame::ApplyBinop(BinaryOperator::Disjunction));
                    command_stack.push(Frame::EvaluateExpr(*lhs));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }
                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Conjunction { lhs, rhs },
                }) => {
                    command_stack.push(Frame::ApplyBinop(BinaryOperator::Conjunction));
                    command_stack.push(Frame::EvaluateExpr(*lhs));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }
                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Equality { lhs, operator, rhs },
                }) => {
                    command_stack.push(Frame::ApplyBinop(operator.value.into()));
                    command_stack.push(Frame::EvaluateExpr(*lhs));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }
                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Comparison { lhs, operator, rhs },
                }) => {
                    command_stack.push(Frame::ApplyBinop(operator.value.into()));
                    command_stack.push(Frame::EvaluateExpr(*lhs));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }
                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Term { lhs, operator, rhs },
                }) => {
                    command_stack.push(Frame::ApplyBinop(operator.value.into()));
                    command_stack.push(Frame::EvaluateExpr(*lhs));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }
                Frame::EvaluateExpr(Expr {
                    span: _,
                    value: ExprType::Factor { lhs, operator, rhs },
                }) => {
                    command_stack.push(Frame::ApplyBinop(operator.value.into()));
                    command_stack.push(Frame::EvaluateExpr(*lhs));
                    command_stack.push(Frame::EvaluateExpr(*rhs));
                }

                // Statements
                Frame::EvaluateStatement(Statement {
                    span: _span,
                    value: statement_type,
                }) => match statement_type {
                    StatementType::Return { expr } => {
                        let call_outer_scope = function_stack.pop().unwrap();
                        let call_end_frame = command_stack
                            .iter()
                            .rev()
                            .position(|frame| matches!(frame, Frame::CallEnd { original_top_scope } if *original_top_scope == call_outer_scope))
                            .unwrap();
                        command_stack.truncate(call_end_frame - 1);

                        command_stack.push(Frame::EvaluateExpr(expr));
                    }
                    StatementType::While { condition, body } => {
                        command_stack.push(Frame::While {
                            condition: *condition.clone(),
                            body: body.clone(),
                        });

                        command_stack.push(Frame::EvaluateExpr(*condition));
                    }
                    StatementType::FunctionDeclaration {
                        ident,
                        params,
                        body,
                    } => {
                        let params = params.into_iter().map(|param| param.value.ident).collect();

                        let function_value = Value::Function {
                            params,
                            body: body.value,
                            defined_in_scope: self.top_scope,
                        };

                        self.top_scope_mut()
                            .values
                            .insert(ident.value.ident, function_value);
                    }
                    StatementType::Decleration { ident, value } => {
                        command_stack.push(Frame::Declare { ident });
                        command_stack.push(Frame::EvaluateExpr(value));
                    }
                    StatementType::DevaluedExpr { expr } => {
                        command_stack.push(Frame::DevalueExpr);
                        command_stack.push(Frame::EvaluateExpr(expr));
                    }
                },

                // Operations
                Frame::ApplyBinop(binary_operator) => {
                    let value_stack_len = value_stack.len();

                    let lhs = value_stack.pop().unwrap();
                    let rhs = value_stack.pop().unwrap();

                    // Simple boolean ops
                    match binary_operator {
                        BinaryOperator::Disjunction => {
                            value_stack.push(
                                Value::Boolean(lhs.as_bool()? || rhs.as_bool()?)
                                    .spanned(lhs.span.start..rhs.span.start),
                            );
                            continue;
                        }
                        BinaryOperator::Conjunction => {
                            value_stack.push(
                                Value::Boolean(lhs.as_bool()? && rhs.as_bool()?)
                                    .spanned(lhs.span.start..rhs.span.start),
                            );
                            continue;
                        }
                        BinaryOperator::Equal => {
                            value_stack.push(
                                Value::Boolean(lhs == rhs).spanned(lhs.span.start..rhs.span.start),
                            );
                            continue;
                        }
                        BinaryOperator::NotEqual => {
                            value_stack.push(
                                Value::Boolean(lhs != rhs).spanned(lhs.span.start..rhs.span.start),
                            );
                            continue;
                        }
                        _ => {}
                    };

                    // Math ops
                    if let BinaryOperator::Plus = binary_operator {
                        if let (Value::String(l), Value::String(r)) = (&lhs.value, &rhs.value) {
                            value_stack.push(Value::String(l.to_owned() + r)
                                .spanned(lhs.span.start..rhs.span.start));
                            continue;
                        } else {
                            check_values_valid_for_math(&lhs, &rhs)?;
                            value_stack.push(apply_op_to_value!(@num lhs, rhs, |l, r| l + r));
                            continue;
                        }
                    }

                    check_values_valid_for_math(&lhs, &rhs)?;
                    value_stack.push(match binary_operator {
                        BinaryOperator::Disjunction
                        | BinaryOperator::Conjunction
                        | BinaryOperator::Equal
                        | BinaryOperator::NotEqual
                        | BinaryOperator::Plus => {
                            unreachable!()
                        }
                        BinaryOperator::GreaterThan => {
                            apply_op_to_value!(@bool lhs, rhs, |l, r| l > r)
                        }
                        BinaryOperator::GreaterThanOrEqual => {
                            apply_op_to_value!(@bool lhs, rhs, |l, r| l >= r)
                        }
                        BinaryOperator::LessThan => {
                            apply_op_to_value!(@bool lhs, rhs, |l, r| l < r)
                        }
                        BinaryOperator::LessThanOrEqual => {
                            apply_op_to_value!(@bool lhs, rhs, |l, r| l <= r)
                        }
                        
                        BinaryOperator::Minus => {
                            apply_op_to_value!(@num lhs, rhs, |l, r| l - r)
                        }
                        BinaryOperator::Multiply => {
                            apply_op_to_value!(@num lhs, rhs, |l, r| l * r)
                        }
                        BinaryOperator::Divide => {
                            apply_op_to_value!(@num lhs, rhs, |l, r| l / r)
                        }
                    });

                    debug_assert!(
                        value_stack_len == value_stack.len() + 1,
                        "Binary operator {:?} should have popped two values from the stack and added one result",
                        binary_operator
                    );
                }
                Frame::ApplyUnaryOp(unary_operator) => {
                    let rhs = value_stack.pop().unwrap();

                    value_stack.push(match unary_operator {
                        UnaryOperator::Negative => {
                            rhs.check_typed_correctly(&[ValueType::Float, ValueType::Integer])?;
                            match rhs.value {
                                Value::Integer(i) => Value::Integer(-i).spanned(rhs.span.clone()),
                                Value::Float(f) => Value::Float(-f).spanned(rhs.span.clone()),
                                _ => unreachable!(),
                            }
                        }
                        UnaryOperator::Negate => {
                            rhs.check_typed_correctly(&[ValueType::Boolean])?;
                            match rhs.value {
                                Value::Boolean(b) => Value::Boolean(!b).spanned(rhs.span.clone()),
                                _ => unreachable!(),
                            }
                        }
                    });
                }
                Frame::CallStart {
                    span,
                    ident,
                    num_args,
                } => {
                    let function = value_stack.pop().unwrap();
                    let mut args = value_stack
                        .drain(value_stack.len() - num_args..)
                        .collect::<Vec<_>>();
                    args.reverse();
                    debug_assert_eq!(args.len(), num_args);

                    fn check_call_arity(
                        ident: String,
                        actual: usize,
                        expected: usize,
                        function_span: Span,
                        arguments_span: Option<Span>,
                    ) -> Result<(), InterpreterError> {
                        if actual != expected {
                            return Err(InterpreterError::IncorrectArgumentCount {
                                ident,
                                expected,
                                actual,
                                function_span,
                                arguments_span,
                            });
                        }
                        Ok(())
                    }

                    match function.value {
                        Value::Function {
                            params: function_params,
                            body: function_body,
                            defined_in_scope,
                        } => {
                            check_call_arity(
                                ident.value.ident,
                                num_args,
                                function_params.len(),
                                span.clone(),
                                args.first().and_then(|start| {
                                    args.last().map(|last| start.span.start..last.span.end)
                                }),
                            )?;

                            let mut new_scope = Scope::new(Some(defined_in_scope));
                            for (param, arg) in function_params.iter().zip(args.into_iter()) {
                                new_scope
                                    .values
                                    .insert(param.to_string(), arg.clone().value);
                            }

                            let original_top_scope = self.top_scope;
                            self.top_scope = defined_in_scope;

                            self.push_scope(new_scope);
                            function_stack.push(original_top_scope);

                            command_stack.push(Frame::CallEnd { original_top_scope });
                            command_stack.push(Frame::EvaluateExpr(
                                ExprType::Block(function_body).spanned(span),
                            ));
                        }
                        Value::NativeFunction { body } => {
                            value_stack.push(
                                body(&args.into_iter().map(|arg| arg.value).collect::<Vec<_>>())?
                                    .spanned(span),
                            );
                        }
                        _ => {
                            return Err(InterpreterError::NotAFunction {
                                ident: ident.value.ident,
                                span,
                            });
                        }
                    }
                }
                Frame::CallEnd { original_top_scope } => {
                    function_stack.pop().unwrap();
                    self.top_scope = original_top_scope;
                }
                Frame::If {
                    span,
                    body,
                    else_body,
                } => {
                    let condition = value_stack.pop().unwrap();

                    let condition = condition.as_bool()?;
                    if condition {
                        command_stack.push(Frame::EvaluateExpr(
                            ExprType::Block(body.value).spanned(span),
                        ));
                    } else if let Some(else_body) = else_body {
                        command_stack.push(Frame::EvaluateExpr(
                            ExprType::Block(else_body.value).spanned(span),
                        ));
                    } else {
                        value_stack.push(Value::Nil.spanned(span));
                    }
                }
                Frame::While { condition, body } => {
                    let condition_res = value_stack.pop().unwrap();
                    let condition_res = condition_res.as_bool()?;

                    if condition_res {
                        command_stack.push(Frame::While {
                            condition: condition.clone(),
                            body: body.clone(),
                        });

                        command_stack.push(Frame::EvaluateExpr(condition));

                        // Loop body and throw away the result of the block
                        command_stack.push(Frame::DevalueExpr);
                        command_stack.push(Frame::EvaluateExpr(
                            ExprType::Block(body.value).spanned(body.span),
                        ));
                    }
                }
                Frame::Array { span, num_elements } => {
                    let elements = value_stack.drain(value_stack.len() - num_elements..).map(|element| element.value).collect::<Vec<_>>();
                    value_stack.push(Value::Array(elements).spanned(span));
                }

                Frame::DevalueExpr => {
                    value_stack.pop();
                }
                Frame::Declare { ident } => {
                    let value = value_stack.pop().unwrap();

                    self.top_scope_mut()
                        .values
                        .insert(ident.value.ident, value.value);
                }
            }
        }

        Ok(())
    }
}
