use std::fmt::Display;

use crate::{Span, parser::Block};

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
    left: &Value,
    right: &Value,
    left_span: Span,
    right_span: Span,
) -> Result<(), InterpreterError> {
    if left.value_type() == right.value_type() {
        Ok(())
    } else {
        Err(InterpreterError::MismatchedTypes {
            lhs: left.value_type(),
            rhs: right.value_type(),
            lhs_span: left_span,
            rhs_span: right_span,
        })
    }
}

pub fn check_values_valid_for_math(
    left: &Value,
    right: &Value,
    left_span: Span,
    right_span: Span,
) -> Result<(), InterpreterError> {
    left.check_typed_correctly(&[ValueType::Float, ValueType::Integer], left_span.clone())?;
    right.check_typed_correctly(&[ValueType::Float, ValueType::Integer], right_span.clone())?;
    check_values_same_type(left, right, left_span, right_span)?;

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
