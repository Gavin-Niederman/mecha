use mecha::{
    error_report::{report_lexer_error, report_parser_error},
    lexer::Lexer,
    parser::Parser,
};

fn main() {
    let input = "if (1*(1+1) / 4 >= (4.2-2) / 4) {
        if (true) {
        };

        1 + 1
    }";

    let mut lexer = Lexer::new(input);
    let lexemes = match lexer.collect() {
        Ok(lexemes) => lexemes,
        Err(e) => {
            report_lexer_error(input, e);
            return;
        }
    };

    let mut parser = Parser::new(&lexemes, input);
    match parser.parse() {
        Ok(ast) => println!("{ast:?}"),
        Err(e) => report_parser_error(input, e),
    }
}
