use crate::expression::Expr;

#[derive(Debug, PartialEq)]

pub enum Statement<'a> {
    Print(PrintStmt<'a>),
    Variable(VariableDeclarationStmt<'a>),
}

impl<'a> Statement<'a> {
    pub fn print(expr: Expr<'a>) -> Self {
        Statement::Print(PrintStmt { expr })
    }

    pub fn variable(ident: &'a str, expr: Expr<'a>) -> Self {
        Statement::Variable(VariableDeclarationStmt { ident, expr })
    }
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
