use std::collections::{BTreeMap, HashMap};

use native::fill_scope_with_native_functions;
use snafu::Snafu;
use value::{Value, ValueType};

pub mod imp;
mod native;
pub mod value;

use crate::{Span, Spanned, parser::Ast};

#[derive(Debug, Clone, PartialEq)]
struct Scope {
    reference_count: u64,
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
            reference_count: 0,
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

        if let Some(parent_scope) = scope.parent_scope {
            let parent = self.scopes.get_mut(&parent_scope).unwrap();
            parent.reference_count += 1;
        }

        self.scopes.insert(id, scope);
        self.top_scope = id;
        self.next_scope_id += 1;
    }
    fn pop_scope(&mut self) -> Option<Scope> {
        let id = self.top_scope;

        let scope = self.scopes.get(&id).unwrap();

        if scope.reference_count != 0 {
            self.top_scope = scope.parent_scope.unwrap_or(0);
            return None;
        }

        let scope = self.scopes.remove(&id).unwrap();

        if let Some(parent_scope) = scope.parent_scope {
            let parent = self.scopes.get_mut(&parent_scope).unwrap();
            parent.reference_count -= 1;
        }
        for value in scope.values.values() {
            if let Value::Function { defined_in_scope, .. } = value {
                if *defined_in_scope == id {
                    continue;
                }
                let defined_in_scope = self.scopes.get_mut(defined_in_scope).unwrap();
                defined_in_scope.reference_count -= 1;
            }
        }
        
        self.top_scope = scope.parent_scope.unwrap_or(0);

        Some(scope)
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
        lhs: Spanned<ValueType>,
        rhs: Spanned<ValueType>,
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
