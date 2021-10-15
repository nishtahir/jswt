use crate::expression::Expr;

#[derive(Debug, PartialEq)]

pub enum Statement<'a> {
    Print(PrintStmt<'a>),
    Variable(VariableDeclarationStmt<'a>),
    Block(BlockStmt<'a>),
}

impl<'a> Statement<'a> {
    pub fn print(expr: Expr<'a>) -> Self {
        Statement::Print(PrintStmt { expr })
    }

    pub fn variable(ident: &'a str, expr: Expr<'a>) -> Self {
        Statement::Variable(VariableDeclarationStmt { ident, expr })
    }

    pub fn block(statements: Vec<Statement<'a>>) -> Self {
        Statement::Block(BlockStmt { statements })
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStmt<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]

pub struct PrintStmt<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarationStmt<'a> {
    pub ident: &'a str,
    pub expr: Expr<'a>,
}
