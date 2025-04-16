# Mecha

A simple language based on Lox from Crafting Interpreters.

## Syntax

Mecha takes aspects of its design from Rust and Lox.

Function can be defined with the `let` keyword:

```
let variable = "hi";
```

Every statement MUST be followed by a semicolon.
Any expression can be turned into a statement by appending a semicolon.

Blocks (every time you see '{...}') are similar to Rust in that adding a trailing expression will make the block evaluate to that expression:

```
let im_equal_to_one = {
    1
};
print(im_equal_to_one);
```

By default blocks will evaluate to `nil`.

The currently supported statements are:

- `return` statements which can only be used inside function blocks.
- `let` statements which define a variable.
- `\` (function) definitions which define a variable and set its value to a function.
- Devalued expressions (any expression followed by `;`)
- `while` expressions

If branches are expressions as both arms are blocks.

Check the [grammar](./grammar) for all of the rules!

### Function expressions

The two definitions are equivalent.

```
\im_a_function() {
    print("hi")
};

let im_another_function = \() {
    print("hi")
};

im_a_function();
im_another_function();
```

## Features

Currently the following native functions are included in the global scope of a mecha program:
- `print`: Prints a value of any type followed by a newline. Takes one argument.
- `type_of`: Returns the type of the argument passed to it. Takes one argument.
- `to_string`: Takes and value and returns the string representation of it. Takes one argument.
- `range`: Returns an array with the range of all integers between the first and second arguments. Takes two integer arguments.
- `random`: Returns a float in the range `[0, 1)`. Takes zero arguments.


## Internals

Mecha is lexed with a greedy and highly optimized token muncher. Every possible token is asked to consume as many tokens as it can match. If it cannot match any, the token with the next lowest priority attempts to match. This priority is important because a token such as `=` would be matched before `==` without it.

Next, Mecha is parsed with a recursive descent parser into an abstract syntax tree. you can find the source code for the most of the parser [here](./src/parser/imp.rs).

Finally, the AST is interpreted. The interpreter utilizes three stacks on the heap to keep track of what function call, if any, is being interpreted, what values have been interpreted from expressions, and what action the interpreter should perform next. This is done in order to avoid stack overflows. Mecha can easily handle functions that recurse hundreds of thousands of times. See [test.mecha](./test.mecha). You can find the implementation of the interpreter [here](./src/interpreter/imp.rs).