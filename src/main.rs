use mecha::{
    error_report::{report_lexer_error, report_parser_error},
    lexer::{Lexeme, Lexer, Token},
    parser::Parser,
};

fn main() {
    let input = "if (1*(1+1) / 4 == (4.2-2) / 4) {
        if (true) {
        };

        1 + 1
    }";

    let mut lexer = Lexer::new(input);
    let mut lexemes = Vec::new();
    loop {
        match lexer.next_lexeme() {
            Ok(
                lexeme @ Lexeme {
                    value: Token::Eoi, ..
                },
            ) => {
                lexemes.push(lexeme);
                break;
            }
            Ok(lexeme) => lexemes.push(lexeme),
            Err(e) => {
                report_lexer_error(input, e);
                return;
            }
        }
    }
    let mut parser = Parser::new(&lexemes, input);
    match parser.parse() {
        Ok(ast) => println!("{ast:?}"),
        Err(e) => report_parser_error(input, e),
    }
}
