use crate::Span;

use super::{InterpreterError, value::Value};

fn print(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.len() == 1, "print function takes exactly one argument");

    let value = &args[0];

    println!("{}", value);

    Ok(Value::Nil)
}

fn insert_native_function(
    scope: &mut crate::interpreter::Scope,
    name: &str,
    num_args: usize,
    function: fn(&[Value]) -> Result<Value, InterpreterError>,
) {
    let value = Value::NativeFunction {
        num_params: num_args,
        body: function,
    };

    scope.values.insert(name.into(), value);
}

pub fn fill_scope_with_native_functions(scope: &mut crate::interpreter::Scope) {
    insert_native_function(scope, "print", 1, print);
}
