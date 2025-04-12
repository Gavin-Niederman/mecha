use std::fmt::Display;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

use crate::{
    interpreter::{InterpreterError, value::ValueType},
    lexer::LexerError,
    parser::ParseError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceFile<'a> {
    pub text: &'a str,
    pub filename: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    LexerInvalidToken,
    ParserUnexpectedEoi,
    ParserUnclosedDeliminator,
    ParserUnexpectedLexeme,
    ParserIfCondMissingParens,
    ParserExprBeforeEndOfBlock,
    InterpreterUnboundIdent,
    InterpreterNotAFunction,
    InterpreterInvalidType,
    InterpreterMismatchedTypes,
    InterpreterIncorrectArgumentCount,
    InterpreterDivideByZero,
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
            ErrorCode::InterpreterUnboundIdent => write!(f, "interpreter:unbound_ident"),
            ErrorCode::InterpreterNotAFunction => write!(f, "interpreter:not_a_function"),
            ErrorCode::InterpreterInvalidType => write!(f, "interpreter:invalid_type"),
            ErrorCode::InterpreterMismatchedTypes => write!(f, "interpreter:mismatched_types"),
            ErrorCode::InterpreterIncorrectArgumentCount => {
                write!(f, "interpreter:incorrect_argument_count")
            }
            ErrorCode::InterpreterDivideByZero => write!(f, "interpreter:divide_by_zero"),
        }
    }
}

fn list_to_string<I: Display>(items: &[I]) -> String {
    let formatted = items
        .iter()
        .map(|item| item.to_string())
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

    Report::build(
        ReportKind::Error,
        (source.filename, error.index..error.index),
    )
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
            list_to_string(&possible_tokens)
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
            Label::new((
                source.filename,
                expected_semicolon_at..expected_semicolon_at,
            ))
            .with_message("Expected semicolon to be here.")
            .with_color(colors.next()),
        )
        .finish(),
    }
    .print((source.filename, Source::from(source.text)))
    .unwrap();
}

struct ValueTypeDisplay(ValueType);
impl Display for ValueTypeDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ValueType::Integer => write!(f, "integer"),
            ValueType::Float => write!(f, "float"),
            ValueType::Boolean => write!(f, "boolean"),
            ValueType::Function => write!(f, "function"),
            ValueType::Nil => write!(f, "nil"),
        }
    }
}
impl From<ValueType> for ValueTypeDisplay {
    fn from(value: ValueType) -> Self {
        Self(value)
    }
}

pub fn report_interpreter_error(source: SourceFile, error: crate::interpreter::InterpreterError) {
    let mut colors = ColorGenerator::new();

    match error {
        InterpreterError::UnboundIdentifier { ident, span } => {
            Report::build(ReportKind::Error, (source.filename, span.clone()))
                .with_code(ErrorCode::InterpreterUnboundIdent)
                .with_message(format!("Used unbound identifier: {ident}"))
                .with_help(
                    "Make sure to define the identifier as a function or variable before using it.",
                )
                .with_label(
                    Label::new((source.filename, span))
                        .with_color(colors.next())
                        .with_message("This identifier is not bound to any value."),
                )
                .finish()
        }
        InterpreterError::NotAFunction { ident, span } => {
            Report::build(ReportKind::Error, (source.filename, span.clone()))
                .with_code(ErrorCode::InterpreterNotAFunction)
                .with_message(format!(
                    "Attempted to call {ident}, which is not a function."
                ))
                .with_help("Make sure to define the identifier as a function before using it.")
                .with_note("Functions can be shadowed by variable definitions!")
                .with_label(
                    Label::new((source.filename, span))
                        .with_color(colors.next())
                        .with_message("This identifier is not bound to a function."),
                )
                .finish()
        }
        InterpreterError::InvalidType {
            valid_types,
            actual_type,
            span,
        } => Report::build(ReportKind::Error, (source.filename, span.clone()))
            .with_code(ErrorCode::InterpreterInvalidType)
            .with_message("An expression evaluated to an invalid type")
            .with_note(format!(
                "Expected a type of one of: {}",
                list_to_string(
                    &valid_types
                        .into_iter()
                        .map(ValueTypeDisplay::from)
                        .collect::<Vec<_>>()
                )
            ))
            .with_label(
                Label::new((source.filename, span))
                    .with_color(colors.next())
                    .with_message(format!(
                        "This expression evaluated to an invalid type: {}",
                        ValueTypeDisplay(actual_type)
                    )),
            )
            .finish(),
        InterpreterError::MismatchedTypes {
            lhs,
            rhs,
        } => Report::build(
            ReportKind::Error,
            (source.filename, lhs.span.start..rhs.span.end),
        )
        .with_code(ErrorCode::InterpreterMismatchedTypes)
        .with_message(format!(
            "The left and right hand sides of an operation are of different types: {} and {}",
            ValueTypeDisplay(lhs.value),
            ValueTypeDisplay(rhs.value)
        ))
        .with_help("Make sure both sides of the operation are of the same type.")
        .with_label(
            Label::new((source.filename, lhs.span))
                .with_color(colors.next())
                .with_message(format!(
                    "This expression evaluated to type: {}",
                    ValueTypeDisplay(lhs.value)
                )),
        )
        .with_label(
            Label::new((source.filename, rhs.span))
                .with_color(colors.next())
                .with_message(format!(
                    "This expression evaluated to type: {}",
                    ValueTypeDisplay(rhs.value)
                )),
        )
        .finish(),
        InterpreterError::IncorrectArgumentCount {
            ident,
            expected,
            actual,
            function_span,
            arguments_span,
        } => {
            let mut report = Report::build(ReportKind::Error, (source.filename, function_span.clone()))
                .with_code(ErrorCode::InterpreterIncorrectArgumentCount)
                .with_message(format!(
                    "Attempted to call {ident} with {actual} arguments, but {ident} takes {expected} arguments."
                ))
                .with_note("Functions must be called with the correct number of arguments.")
                .with_label(
                    Label::new((source.filename, function_span))
                        .with_color(colors.next())
                        .with_message(format!("This function takes {expected} arguments.")),
                );
            if let Some(arguments_span) = arguments_span {
                report = report.with_label(
                    Label::new((source.filename, arguments_span))
                        .with_color(colors.next())
                        .with_message(format!("This function was called with {actual} arguments.")),
                );
            }
            report.finish()
        },
        InterpreterError::DivideByZero { span } => {
            Report::build(ReportKind::Error, (source.filename, span.clone()))
                .with_code(ErrorCode::InterpreterDivideByZero)
                .with_message("Division by zero occured.".to_string())
                .with_help("When possible, check for division by zero.")
                .with_label(
                    Label::new((source.filename, span))
                        .with_color(colors.next())
                        .with_message("Division by zero occured here."),
                )
                .finish()
        },
    }
    .print((source.filename, Source::from(source.text)))
    .unwrap();
}
