#![feature(test)]

extern crate test;

pub mod error_report;
pub mod lexer;
pub mod parser;
pub mod visualize_ast;
pub mod interpreter;

pub type Span = std::ops::Range<usize>;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}
impl<T> Spanned<T> {
    pub const fn new(span: Span, value: T) -> Spanned<T> {
        Spanned { span, value }
    }
    pub fn into_inner(self) -> T {
        self.value
    }
}
