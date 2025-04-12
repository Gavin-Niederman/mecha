use std::fmt::Display;

use crate::{Spanned, lexer::Token};

pub type Ast = Vec<Statement>;

pub type Statement = Spanned<StatementType>;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementType {
    Return { expr: Expr },
    FunctionDeclaration {
        ident: Spanned<Identifier>,
        params: Vec<Spanned<Identifier>>,
        body: Spanned<Block>,
    },
    Decleration { ident: Spanned<Identifier>, value: Expr },
    DevaluedExpr { expr: Expr },
}

pub type Expr = Spanned<ExprType>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    If {
        condition: Box<Expr>,
        body: Spanned<Block>,
        else_body: Option<Spanned<Block>>,
    },

    Equality {
        lhs: Box<Expr>,
        operator: Spanned<EqualityOperator>,
        rhs: Box<Expr>,
    },
    Comparison {
        lhs: Box<Expr>,
        operator: Spanned<ComparisonOperator>,
        rhs: Box<Expr>,
    },

    Disjunction {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Conjunction {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Term {
        lhs: Box<Expr>,
        operator: Spanned<TermOperator>,
        rhs: Box<Expr>,
    },
    Factor {
        lhs: Box<Expr>,
        operator: Spanned<FactorOperator>,
        rhs: Box<Expr>,
    },

    Unary {
        operator: Spanned<UnaryOperator>,
        rhs: Box<Expr>,
    },

    Call {
        ident: Spanned<Identifier>,
        params: Vec<Expr>
    },
    Terminal(Terminal),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminal {
    Nil,
    Boolean(bool),
    Float(f64),
    Integer(i64),
    Closure {
        params: Vec<Spanned<Identifier>>,
        body: Spanned<Block>,
    },
    Ident(Identifier)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub ret: Option<Box<Expr>>,
} 
impl From<Spanned<Block>> for Expr {
    fn from(value: Spanned<Block>) -> Self {
        Expr {
            span: value.span,
            value: ExprType::Block(value.value)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub ident: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}
impl TryFrom<Token> for EqualityOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Equality => Ok(Self::Equal),
            Token::NotEquality => Ok(Self::NotEqual),
            _ => Err(()),
        }
    }
}
impl Display for EqualityOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EqualityOperator::Equal =>  write!(f, "'=='"),
            EqualityOperator::NotEqual =>  write!(f, "'!='"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOperator {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}
impl TryFrom<Token> for ComparisonOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::GreaterThan => Ok(Self::GreaterThan),
            Token::GreaterThanOrEqual => Ok(Self::GreaterThanOrEqual),
            Token::LessThan => Ok(Self::LessThan),
            Token::LessThanOrEqual => Ok(Self::LessThanOrEqual),
            _ => Err(()),
        }
    }
}
impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonOperator::GreaterThan =>  write!(f, "'>'"),
            ComparisonOperator::GreaterThanOrEqual =>  write!(f, "'>='"),
            ComparisonOperator::LessThan =>  write!(f, "'<'"),
            ComparisonOperator::LessThanOrEqual =>  write!(f, "'<='"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TermOperator {
    Plus,
    Minus,
}
impl TryFrom<Token> for TermOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Self::Plus),
            Token::Minus => Ok(Self::Minus),
            _ => Err(()),
        }
    }
}
impl Display for TermOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TermOperator::Plus => write!(f, "'+'"),
            TermOperator::Minus => write!(f, "'-'"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FactorOperator {
    Multiply,
    Divide,
}
impl TryFrom<Token> for FactorOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Star => Ok(Self::Multiply),
            Token::Slash => Ok(Self::Divide),
            _ => Err(()),
        }
    }
}
impl Display for FactorOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FactorOperator::Multiply => write!(f, "'*'"),
            FactorOperator::Divide => write!(f, "'/'"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Negative,
    Negate,
}
impl TryFrom<Token> for UnaryOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Minus => Ok(Self::Negative),
            Token::Bang => Ok(Self::Negate),
            _ => Err(()),
        }
    }
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Negative => write!(f, "'-'"),
            UnaryOperator::Negate => write!(f, "'!'"),
        }
    }
}
