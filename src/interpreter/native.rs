use super::{InterpreterError, value::Value};

//TODO: Better error handling!!!!!!

fn print(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.len() == 1, "print function takes exactly one argument");

    let value = &args[0];

    println!("{}", value);

    Ok(Value::Nil)
}
fn random(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.is_empty(), "random function takes no arguments");

    let value = rand::random::<f64>();

    Ok(Value::Float(value))
}

fn insert_native_function(
    scope: &mut crate::interpreter::Scope,
    name: &str,
    function: fn(&[Value]) -> Result<Value, InterpreterError>,
) {
    let value = Value::NativeFunction {
        body: function,
    };

    scope.values.insert(name.into(), value);
}

pub fn fill_scope_with_native_functions(scope: &mut crate::interpreter::Scope) {
    insert_native_function(scope, "print",  print);
    insert_native_function(scope, "random", random);
}
