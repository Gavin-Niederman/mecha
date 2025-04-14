use super::{InterpreterError, value::Value};

//TODO: Better error handling!!!!!!

fn print(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.len() == 1, "print function takes exactly one argument");

    let value = &args[0];

    println!("{}", value);

    Ok(Value::Nil)
}
fn type_of(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.len() == 1, "type_of function takes exactly one argument");

    let value = &args[0];

    let value_type = value.value_type();

    Ok(Value::Type(value_type))
}
fn to_string(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.len() == 1, "to_string function takes exactly one argument");

    let value = &args[0];

    Ok(Value::String(value.to_string()))
}
fn range(args: &[Value]) -> Result<Value, InterpreterError> {
    assert!(args.len() == 2, "range function takes exactly two arguments");

    let (Value::Integer(start), Value::Integer(end)) = (args[0].clone(), args[1].clone()) else {
        panic!("arguments to range must be integers");
    };

    let range: Vec<Value> = (start..=end).map(Value::Integer).collect();

    Ok(Value::Array(range))
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
    insert_native_function(scope, "type_of", type_of);
    insert_native_function(scope, "to_string", to_string);
    insert_native_function(scope, "range", range);
    insert_native_function(scope, "random", random);
}
