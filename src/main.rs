use std::{fs::File, path::PathBuf};

use clap::Parser as _;
use mecha::{
    error_report::{report_lexer_error, report_parser_error, SourceFile}, interpreter::Interpreter, lexer::Lexer, parser::Parser, visualize_ast
};

#[derive(clap::Parser, Debug, Clone)]
#[command(version, about, author)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Clone, clap::Subcommand)]
enum Command {
    Run { file: PathBuf },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Run { file } => {
            let filename = file
                .file_name()
                .map(|name| name.to_str().unwrap())
                .unwrap_or("source_code")
                .to_owned();
            let text = std::fs::read_to_string(file).unwrap();
            let source_file = SourceFile {
                text: &text,
                filename: &filename,
            };

            let mut lexer = Lexer::new(&text);
            let lexemes = match lexer.collect() {
                Ok(lexemes) => lexemes,
                Err(e) => {
                    report_lexer_error(source_file, e);
                    return;
                }
            };

            let mut parser = Parser::new(&lexemes, &text);
            let ast = match parser.parse() {
                Ok(ast) => ast,
                Err(e) => {
                    report_parser_error(source_file, e);
                    return;
                }
            };

            let mut graph_file = File::create("ast.dot").unwrap();
            visualize_ast::render_to(ast.clone(), &mut graph_file).unwrap();

            let mut interpreter = Interpreter::new(ast);
            interpreter.interpret().unwrap();
        }
    }
}
