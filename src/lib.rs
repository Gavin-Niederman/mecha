#![feature(test)]

use std::borrow::Borrow;

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
impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T {
        &self.value
    }
}
trait Spannable {
    fn spanned(self, span: Span) -> Spanned<Self>
    where
        Self: Sized;
}
impl<T> Spannable for T {
    fn spanned(self, span: Span) -> Spanned<Self> {
        Spanned::new(span, self)
    }
}