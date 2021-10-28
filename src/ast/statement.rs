use super::{
    expression::Expr,
    function::{FunctionStmt, Param},
    ident::Ident,
    span::Span,
    types::Type,
};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Print { field1: PrintStmt<'a> },
    Variable(VariableDeclarationStmt<'a>),
    Block(BlockStmt<'a>),
    Function(FunctionStmt<'a>),
}

impl<'a> Statement<'a> {
    pub fn print(expr: Expr<'a>, start: usize, end: usize) -> Self {
        Statement::Print { field1: PrintStmt {
            expr,
            span: Span::new(start, end),
        } }
    }

    pub fn variable(ident: &'a str, expr: Expr<'a>, start: usize, end: usize) -> Self {
        Statement::Variable(VariableDeclarationStmt {
            span: Span::new(start, end),
            ident,
            expr,
        })
    }

    pub fn block(statements: Vec<Statement<'a>>) -> Self {
        Statement::Block(BlockStmt { statements })
    }

    pub fn function(
        ident: Ident<'a>,
        params: Vec<Param<'a>>,
        returns: Type<'a>,
        body: Statement<'a>,
    ) -> Self {
        Statement::Function(FunctionStmt {
            ident,
            params,
            returns,
        })
    }

    /// Returns `true` if the statement is [`Function`].
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }

    pub fn as_function(&self) -> Option<&FunctionStmt<'a>> {
        if let Self::Function(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStmt<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]

pub struct PrintStmt<'a> {
    pub span: Span,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarationStmt<'a> {
    pub span: Span,
    pub ident: &'a str,
    pub expr: Expr<'a>,
}
