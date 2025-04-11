use std::collections::{BTreeMap, HashMap};

use native::fill_scope_with_native_functions;
use snafu::Snafu;
use value::{Value, ValueType, check_values_valid_for_math};

mod native;
pub mod value;

use crate::{
    Span,
    parser::{
        Ast, Block, ComparisonOperator, EqualityOperator, Expr, ExprType, FactorOperator,
        Statement, StatementType, TermOperator, Terminal, UnaryOperator,
    },
};

#[derive(Debug, Clone, PartialEq)]
struct Scope {
    parent_scope: Option<u64>,
    values: HashMap<String, Value>,
}
impl Scope {
    fn get(&self, ident: &str, span: Span) -> Result<Value, InterpreterError> {
        self.values
            .get(ident)
            .cloned()
            .ok_or(InterpreterError::UnboundIdentifier {
                ident: ident.to_string(),
                span,
            })
    }

    fn new(parent: Option<u64>) -> Self {
        Self {
            parent_scope: parent,
            values: HashMap::new(),
        }
    }
}

pub struct Interpreter {
    next_scope_id: u64,
    top_scope: u64,
    scopes: BTreeMap<u64, Scope>,
    ast: Ast,
}

macro_rules! apply_op_to_value {
    (@num $lhs:ident, $rhs:ident, |$l:ident, $r:ident| $body:expr) => {
        match ($lhs, $rhs) {
            (Value::Integer($l), Value::Integer($r)) => Ok(Value::Integer($body)),
            (Value::Float($l), Value::Float($r)) => Ok(Value::Float($body)),
            _ => unreachable!(),
        }
    };
    (@bool $lhs:ident, $rhs:ident, |$l:ident, $r:ident| $body:expr) => {
        match ($lhs, $rhs) {
            (Value::Integer($l), Value::Integer($r)) => Ok(Value::Boolean($body)),
            (Value::Float($l), Value::Float($r)) => Ok(Value::Boolean($body)),
            _ => unreachable!(),
        }
    };
}

impl Interpreter {
    pub fn new(ast: Ast) -> Self {
        let mut this = Self {
            next_scope_id: 0,
            top_scope: 0,
            scopes: BTreeMap::new(),
            ast,
        };

        let mut global_scope = Scope::new(None);
        fill_scope_with_native_functions(&mut global_scope);
        this.push_scope(global_scope);

        this
    }

    pub fn interpret(&mut self) -> Result<(), InterpreterError> {
        let ast = self.ast.clone();

        for statement in ast {
            self.visit_statement(statement)?;
        }

        Ok(())
    }

    /// Collects all accessible identifiers into a single super scope
    fn super_scope(&self) -> Scope {
        let mut scope = Scope::new(None);

        let mut top_scope = self.top_scope();
        loop {
            for value in top_scope.values.iter() {
                if scope.values.contains_key(value.0) {
                    continue;
                }
                scope.values.insert(value.0.clone(), value.1.clone());
            }

            let Some(parent_scope) = top_scope.parent_scope else {
                break;
            };

            let parent_scope = self.scopes.get(&parent_scope).unwrap();
            top_scope = parent_scope;
        }

        scope
    }

    fn push_scope(&mut self, scope: Scope) {
        let id = self.next_scope_id;
        self.scopes.insert(id, scope);
        self.top_scope = id;
        self.next_scope_id += 1;
    }
    fn pop_scope(&mut self) -> Scope {
        let id = self.top_scope;
        let scope = self.scopes.remove(&id).unwrap();
        self.top_scope = scope.parent_scope.unwrap_or(0);
        scope
    }
    fn top_scope(&self) -> &Scope {
        self.scopes
            .get(&self.top_scope)
            .expect("Top scope should always exist")
    }
    fn top_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .get_mut(&self.top_scope)
            .expect("Top scope should always exist")
    }

    fn visit_expr(&mut self, expr: Expr) -> Result<Value, InterpreterError> {
        let super_scope = self.super_scope();

        match expr.value {
            ExprType::If {
                condition,
                body,
                else_body,
            } => {
                let condition_value = self.visit_expr(*condition.clone())?;
                condition_value.check_typed_correctly(&[ValueType::Boolean], condition.span)?;
                let Value::Boolean(condition) = condition_value else {
                    unreachable!()
                };

                if condition {
                    self.visit_expr(body.into())
                } else if let Some(else_body) = else_body {
                    self.visit_expr(else_body.into())
                } else {
                    Ok(Value::Nil)
                }
            }
            ExprType::Equality { lhs, operator, rhs } => {
                let lhs_value = self.visit_expr(*lhs.clone())?;
                let rhs_value = self.visit_expr(*rhs.clone())?;

                match operator.value {
                    EqualityOperator::Equal => Ok(Value::Boolean(lhs_value == rhs_value)),
                    EqualityOperator::NotEqual => Ok(Value::Boolean(lhs_value != rhs_value)),
                }
            }
            ExprType::Comparison { lhs, operator, rhs } => {
                let lhs_value = self.visit_expr(*lhs.clone())?;
                let rhs_value = self.visit_expr(*rhs.clone())?;
                check_values_valid_for_math(&lhs_value, &rhs_value, lhs.span, rhs.span)?;

                match operator.value {
                    ComparisonOperator::GreaterThan => {
                        apply_op_to_value!(@bool lhs_value, rhs_value, |l, r| l > r)
                    }
                    ComparisonOperator::GreaterThanOrEqual => {
                        apply_op_to_value!(@bool lhs_value, rhs_value, |l, r| l >= r)
                    }
                    ComparisonOperator::LessThan => {
                        apply_op_to_value!(@bool lhs_value, rhs_value, |l, r| l < r)
                    }
                    ComparisonOperator::LessThanOrEqual => {
                        apply_op_to_value!(@bool lhs_value, rhs_value, |l, r| l <= r)
                    }
                }
            }
            ExprType::Disjunction { lhs, rhs } => {
                let lhs_value = self.visit_expr(*lhs.clone())?;
                let rhs_value = self.visit_expr(*rhs.clone())?;
                lhs_value.check_typed_correctly(&[ValueType::Boolean], lhs.span)?;
                rhs_value.check_typed_correctly(&[ValueType::Boolean], rhs.span)?;

                let (Value::Boolean(l), Value::Boolean(r)) = (lhs_value, rhs_value) else {
                    unreachable!()
                };
                Ok(Value::Boolean(l || r))
            }
            ExprType::Conjunction { lhs, rhs } => {
                let lhs_value = self.visit_expr(*lhs.clone())?;
                let rhs_value = self.visit_expr(*rhs.clone())?;
                lhs_value.check_typed_correctly(&[ValueType::Boolean], lhs.span)?;
                rhs_value.check_typed_correctly(&[ValueType::Boolean], rhs.span)?;

                let (Value::Boolean(l), Value::Boolean(r)) = (lhs_value, rhs_value) else {
                    unreachable!()
                };
                Ok(Value::Boolean(l && r))
            }
            ExprType::Term { lhs, operator, rhs } => {
                let lhs_value = self.visit_expr(*lhs.clone())?;
                let rhs_value = self.visit_expr(*rhs.clone())?;
                check_values_valid_for_math(&lhs_value, &rhs_value, lhs.span, rhs.span)?;

                match operator.value {
                    TermOperator::Plus => {
                        apply_op_to_value!(@num lhs_value, rhs_value, |l, r| l + r)
                    }
                    TermOperator::Minus => {
                        apply_op_to_value!(@num lhs_value, rhs_value, |l, r| l - r)
                    }
                }
            }
            ExprType::Factor { lhs, operator, rhs } => {
                let lhs_value = self.visit_expr(*lhs.clone())?;
                let rhs_value = self.visit_expr(*rhs.clone())?;
                check_values_valid_for_math(&lhs_value, &rhs_value, lhs.span, rhs.clone().span)?;

                match operator.value {
                    FactorOperator::Multiply => {
                        apply_op_to_value!(@num lhs_value, rhs_value, |l, r| l * r)
                    }
                    FactorOperator::Divide => {
                        if rhs_value == Value::Integer(0) || rhs_value == Value::Float(0.0) {
                            return Err(InterpreterError::DivideByZero { span: rhs.span });
                        }

                        apply_op_to_value!(@num lhs_value, rhs_value, |l, r| l / r)
                    }
                }
            }
            ExprType::Unary { operator, rhs } => {
                let rhs_value = self.visit_expr(*rhs.clone())?;

                match operator.value {
                    UnaryOperator::Negative => {
                        rhs_value.check_typed_correctly(
                            &[ValueType::Float, ValueType::Integer],
                            rhs.span,
                        )?;
                        match rhs_value {
                            Value::Integer(i) => Ok(Value::Integer(-i)),
                            Value::Float(f) => Ok(Value::Float(-f)),
                            _ => unreachable!(),
                        }
                    }
                    UnaryOperator::Negate => {
                        rhs_value.check_typed_correctly(&[ValueType::Boolean], rhs.span)?;
                        match rhs_value {
                            Value::Boolean(b) => Ok(Value::Boolean(!b)),
                            _ => unreachable!(),
                        }
                    }
                }
            }
            ExprType::Call {
                ident,
                params: call_params,
            } => {
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

                match super_scope.get(&ident.value.ident, ident.span)? {
                    Value::Function {
                        params: function_params,
                        body: function_body,
                        defined_in_scope,
                    } => {
                        check_call_arity(
                            ident.value.ident,
                            call_params.len(),
                            function_params.len(),
                            expr.span,
                            call_params.first().and_then(|start| {
                                call_params
                                    .last()
                                    .map(|last| start.span.start..last.span.end)
                            }),
                        )?;

                        let mut new_scope = Scope::new(Some(defined_in_scope));
                        for (param, arg) in function_params.iter().zip(call_params.iter()) {
                            let value = self.visit_expr(arg.clone())?;
                            new_scope.values.insert(param.clone(), value);
                        }

                        let old_top_scope = self.top_scope;
                        self.top_scope = defined_in_scope;

                        self.push_scope(new_scope);

                        let return_value = self.visit_block(function_body)?;
                        self.pop_scope();
                        self.top_scope = old_top_scope;

                        Ok(return_value)
                    }
                    Value::NativeFunction { num_params, body } => {
                        check_call_arity(
                            ident.value.ident,
                            call_params.len(),
                            num_params,
                            expr.span,
                            call_params.first().and_then(|start| {
                                call_params
                                    .last()
                                    .map(|last| start.span.start..last.span.end)
                            }),
                        )?;

                        body(
                            &call_params
                                .into_iter()
                                .map(|param| self.visit_expr(param))
                                .collect::<Result<Vec<_>, _>>()?,
                        )
                    }
                    _ => Err(InterpreterError::NotAFunction {
                        ident: ident.value.ident,
                        span: expr.span,
                    }),
                }
            }
            ExprType::Terminal(terminal) => match terminal {
                Terminal::Nil => Ok(Value::Nil),
                Terminal::Boolean(b) => Ok(Value::Boolean(b)),
                Terminal::Float(f) => Ok(Value::Float(f)),
                Terminal::Integer(i) => Ok(Value::Integer(i)),
                Terminal::Ident(ident) => super_scope.get(&ident.ident, expr.span),
            },
            ExprType::Block(block) => self.visit_block(block),
        }
    }

    fn visit_block(&mut self, block: Block) -> Result<Value, InterpreterError> {
        self.push_scope(Scope::new(Some(self.top_scope)));
        for statement in block.statements {
            self.visit_statement(statement)?;
        }

        let return_value = if let Some(ret) = block.ret {
            self.visit_expr(*ret)?
        } else {
            Value::Nil
        };

        self.pop_scope();

        Ok(return_value)
    }

    fn visit_statement(&mut self, statement: Statement) -> Result<(), InterpreterError> {
        match statement.value {
            StatementType::Return { expr: _expr } => todo!(),
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
                let value = self.visit_expr(value)?;
                self.top_scope_mut().values.insert(ident.value.ident, value);
            }
            StatementType::DevaluedExpr { expr } => {
                self.visit_expr(expr)?;
            }
        };

        Ok(())
    }
}

#[derive(Snafu, Debug)]
pub enum InterpreterError {
    #[snafu(display("Unbound identifier: {ident}"))]
    UnboundIdentifier { ident: String, span: Span },

    #[snafu(display("Attempted to call {ident} which is not a function."))]
    NotAFunction { ident: String, span: Span },
    #[snafu(display(
        "Required expression to evaluate to one of the types: {valid_types:?}, but it was {actual_type:?}."
    ))]
    InvalidType {
        valid_types: Vec<ValueType>,
        actual_type: ValueType,
        span: Span,
    },
    #[snafu(display("Mismatched types: {lhs:?} and {rhs:?}."))]
    MismatchedTypes {
        lhs: ValueType,
        rhs: ValueType,
        lhs_span: Span,
        rhs_span: Span,
    },

    #[snafu(display("Called {ident} with an incorrect number of arguments."))]
    IncorrectArgumentCount {
        ident: String,
        expected: usize,
        actual: usize,
        function_span: Span,
        arguments_span: Option<Span>,
    },

    #[snafu(display("Divide by zero error."))]
    DivideByZero { span: Span },
}
