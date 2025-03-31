use crate::{Spanned, lexer::Token};

pub type Statement = Spanned<StatementType>;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementType {
    Return { expr: Expr },
    DevaluedExpr { expr: Expr },
}

pub type Expr = Spanned<ExprType>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    If {
        condition: Box<Expr>,
        body: Box<Expr>,
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

    Terminal(Terminal),
    Block {
        statements: Vec<Statement>,
        ret: Option<Box<Expr>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Terminal {
    Boolean(bool),
    Float(f64),
    Integer(i64),
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
            Token::Equal => Ok(Self::Equal),
            Token::NotEqual => Ok(Self::NotEqual),
            _ => Err(()),
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
