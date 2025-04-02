use std::fmt::Display;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

use crate::{
    lexer::{LexerError, Token},
    parser::ParseError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceFile<'a> {
    pub text: &'a str,
    pub filename: &'a str
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    LexerInvalidToken,
    ParserUnexpectedEoi,
    ParserUnclosedDeliminator,
    ParserUnexpectedLexeme,
    ParserIfCondMissingParens,
    ParserExprBeforeEndOfBlock,
}
impl Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorCode::LexerInvalidToken => write!(f, "lexer:invalid_token"),
            ErrorCode::ParserUnclosedDeliminator => write!(f, "parser:unclosed_deliminator"),
            ErrorCode::ParserUnexpectedEoi => write!(f, "parser:unexpected_eoi"),
            ErrorCode::ParserUnexpectedLexeme => write!(f, "parser:unexpected_lexeme"),
            ErrorCode::ParserIfCondMissingParens => write!(f, "parser:if_condition_missing_parens"),
            ErrorCode::ParserExprBeforeEndOfBlock => {
                write!(f, "parser:expression_before_end_of_block")
            }
        }
    }
}

fn token_list_to_string(tokens: &[Token]) -> String {
    let formatted = tokens
        .iter()
        .map(|token| token.to_string())
        .collect::<Vec<_>>();

    let mut final_string = String::new();

    for (i, string) in formatted.iter().enumerate() {
        final_string.push_str(string);
        if i < formatted.len() - 1 {
            final_string.push_str(", ");
        }
    }

    final_string
}

pub fn report_lexer_error(source: SourceFile, error: LexerError) {
    let mut colors = ColorGenerator::new();
    let label_color = colors.next();

    Report::build(ReportKind::Error, (source.filename, error.index..error.index))
        .with_code(ErrorCode::LexerInvalidToken)
        .with_message("Invalid token found in input.")
        .with_label(
            Label::new((source.filename, error.index..error.index))
                .with_message("Invalid token starts here.")
                .with_color(label_color),
        )
        .with_help("Check for invalid characters or malformed keywords and operators")
        .finish()
        .print((source.filename, Source::from(source.text)))
        .unwrap();
}

pub fn report_parser_error(source: SourceFile, error: ParseError) {
    let mut colors = ColorGenerator::new();

    match error {
        ParseError::Multiple { errors } => {
            for error in errors {
                report_parser_error(source, error);
            }
            return;
        }

        //UNEXPECTED LEXEME
        ParseError::UnexpectedLexeme {
            unexpected_lexeme,
            possible_tokens,
        } => Report::build(
            ReportKind::Error,
            (source.filename, unexpected_lexeme.span.clone()),
        )
        .with_code(ErrorCode::ParserUnexpectedLexeme)
        .with_message(format!(
            "Unexpected {} encountered.",
            unexpected_lexeme.value
        ))
        .with_label(
            Label::new((source.filename, unexpected_lexeme.span))
                .with_message("Unexpected input found here.")
                .with_color(colors.next()),
        )
        .with_note(format!(
            "Expected one of: {}",
            token_list_to_string(&possible_tokens)
        ))
        .with_help("Check if your syntax is valid.")
        .finish(),

        //UNEXPECTED EOI
        ParseError::UnexpectedEndOfInput { lexeme } => {
            Report::build(ReportKind::Error, (source.filename, lexeme.span.clone()))
                .with_code(ErrorCode::ParserUnexpectedEoi)
                .with_label(
                    Label::new((source.filename, lexeme.span.end..lexeme.span.end))
                        .with_message("Unexpected end of input after this character.")
                        .with_color(colors.next()),
                )
                .finish()
        }

        //UNCLOSED DELIM
        ParseError::UnclosedDelimiter { delimiter, start } => {
            Report::build(ReportKind::Error, (source.filename, start.span.clone()))
                .with_code(ErrorCode::ParserUnclosedDeliminator)
                .with_message("Unclosed delimiter found.")
                .with_label(
                    Label::new((source.filename, start.span))
                        .with_color(colors.next())
                        .with_message(format!("Unclosed delimiter starts at this {}", delimiter)),
                )
                .with_help("Try closing the delimiter properly")
                .finish()
        }

        //IF MISSING PAREN AROUND COND
        ParseError::IfConditionLackingParens {
            if_lexeme,
            expected_paren_at,
        } => Report::build(ReportKind::Error, (source.filename, if_lexeme.span.clone()))
            .with_code(ErrorCode::ParserIfCondMissingParens)
            .with_message("If condition is missing parentheses")
            .with_label(
                Label::new((source.filename, if_lexeme.span))
                    .with_color(colors.next())
                    .with_message("This if's condition is not wrapped in parens."),
            )
            .with_label(
                Label::new((source.filename, expected_paren_at..expected_paren_at))
                    .with_color(colors.next())
                    .with_message("Expected parens to start here."),
            )
            .with_help("Wrap your condition in parens.")
            .finish(),

        //BLOCK NON TRAILING EXPR
        ParseError::BlockNonTrailingExpr {
            block_start,
            expr_span,
        } => Report::build(
            ReportKind::Error,
            (source.filename, block_start..expr_span.end),
        )
        .with_code(ErrorCode::ParserExprBeforeEndOfBlock)
        .with_message("Expression found before the end of a block.")
        .with_note("Tailing expressions can only go at the end of a block.")
        .with_help("Try adding a semicolon to make the expression a statement.")
        .with_label(
            Label::new((source.filename, block_start..block_start))
                .with_message("In this block.")
                .with_color(colors.next()),
        )
        .with_label(
            Label::new((source.filename, expr_span))
                .with_message("This expression is not in the trailing position.")
                .with_color(colors.next()),
        )
        .finish(),

        //MISSING SEMICOLON
        ParseError::MissingSemicolon {
            statement_span,
            expected_semicolon_at,
        } => Report::build(
            ReportKind::Error,
            (source.filename, statement_span.start..expected_semicolon_at),
        )
        .with_code(ErrorCode::ParserExprBeforeEndOfBlock)
        .with_message("Missing semicolon at the end of a statement.")
        .with_help("Add a semicolon to the end of the statement.")
        .with_label(
            Label::new((source.filename, statement_span))
                .with_message("Missing semicolon here.")
                .with_color(colors.next()),
        )
        .with_label(
            Label::new((source.filename, expected_semicolon_at..expected_semicolon_at))
                .with_message("Expected semicolon to be here.")
                .with_color(colors.next()),
        )
        .finish(),
    }
    .print((source.filename, Source::from(source.text)))
    .unwrap();
}
