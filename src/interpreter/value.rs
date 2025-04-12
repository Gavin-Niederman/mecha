use std::fmt::Display;

use crate::{Span, Spanned, parser::Block};

use super::InterpreterError;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Float(f64),
    Integer(i64),
    Function {
        params: Vec<String>,
        body: Block,
        defined_in_scope: u64,
    },
    NativeFunction {
        num_params: usize,
        body: fn(&[Value]) -> Result<Value, InterpreterError>,
    },
}
impl Value {
    pub const fn value_type(&self) -> ValueType {
        match self {
            Value::Nil => ValueType::Nil,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Float(_) => ValueType::Float,
            Value::Integer(_) => ValueType::Integer,
            Value::Function { .. } => ValueType::Function,
            Value::NativeFunction { .. } => ValueType::Function,
        }
    }

    pub fn check_typed_correctly(
        &self,
        valid_types: &[ValueType],
        span: Span,
    ) -> Result<(), InterpreterError> {
        let value_type = self.value_type();
        valid_types
            .contains(&value_type)
            .then_some(())
            .ok_or(InterpreterError::InvalidType {
                valid_types: valid_types.to_vec(),
                actual_type: value_type,
                span,
            })
    }

    pub fn as_bool(&self, span: Span) -> Result<bool, InterpreterError> {
        match self {
            Value::Boolean(b) => Ok(*b),
            _ => Err(InterpreterError::InvalidType {
                valid_types: vec![ValueType::Boolean],
                actual_type: self.value_type(),
                span,
            }),
        }
    }
    pub fn as_float(&self, span: Span) -> Result<f64, InterpreterError> {
        match self {
            Value::Float(b) => Ok(*b),
            _ => Err(InterpreterError::InvalidType {
                valid_types: vec![ValueType::Float],
                actual_type: self.value_type(),
                span,
            }),
        }
    }
    pub fn as_integer(&self, span: Span) -> Result<i64, InterpreterError> {
        match self {
            Value::Integer(b) => Ok(*b),
            _ => Err(InterpreterError::InvalidType {
                valid_types: vec![ValueType::Integer],
                actual_type: self.value_type(),
                span,
            }),
        }
    }
}
impl Spanned<Value> {
    pub const fn value_type(&self) -> ValueType {
        self.value.value_type()
    }

    pub fn check_typed_correctly(&self, valid_types: &[ValueType]) -> Result<(), InterpreterError> {
        self.value
            .check_typed_correctly(valid_types, self.span.clone())
    }
    pub fn as_bool(&self) -> Result<bool, InterpreterError> {
        self.value.as_bool(self.span.clone())
    }
    pub fn into_float(self) -> Result<f64, InterpreterError> {
        self.value.as_float(self.span.clone())
    }
    pub fn into_integer(self) -> Result<i64, InterpreterError> {
        self.value.as_integer(self.span.clone())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Function { params, .. } => {
                write!(f, "<function({})>", params.join(", "))
            }
            Value::NativeFunction { num_params, .. } => {
                write!(f, "<native function({})>", num_params)
            }
        }
    }
}

pub fn check_values_same_type(
    left: &Spanned<Value>,
    right: &Spanned<Value>,
) -> Result<(), InterpreterError> {
    if left.value_type() == right.value_type() {
        Ok(())
    } else {
        Err(InterpreterError::MismatchedTypes {
            lhs: Spanned {
                span: left.span.clone(),
                value: left.value_type(),
            },
            rhs: Spanned {
                span: right.span.clone(),
                value: right.value_type(),
            },
        })
    }
}

pub fn check_values_valid_for_math(
    left: &Spanned<Value>,
    right: &Spanned<Value>,
) -> Result<(), InterpreterError> {
    left.check_typed_correctly(&[ValueType::Float, ValueType::Integer])?;
    right.check_typed_correctly(&[ValueType::Float, ValueType::Integer])?;
    check_values_same_type(left, right)?;

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Nil,
    Boolean,
    Float,
    Integer,
    Function,
}
