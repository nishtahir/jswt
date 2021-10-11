use crate::expression::Expr;

#[derive(Debug, PartialEq)]

pub enum Statement<'a> {
    Print(PrintStmt<'a>),
    Variable(VariableDeclarationStmt<'a>),
}

#[derive(Debug, PartialEq)]

pub struct PrintStmt<'a> {
    pub expr: Expr<'a>,
}

impl<'a> PrintStmt<'a> {
    pub fn new(expr: Expr<'a>) -> Self {
        Self { expr }
    }
}

impl<'a> From<PrintStmt<'a>> for Statement<'a> {
    fn from(expr: PrintStmt<'a>) -> Self {
        Statement::Print(expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarationStmt<'a> {
    pub ident: &'a str,
    pub expr: Expr<'a>,
}

impl<'a> VariableDeclarationStmt<'a> {
    pub fn new(ident: &'a str, expr: Expr<'a>) -> Self {
        Self { ident, expr }
    }
}

impl<'a> From<VariableDeclarationStmt<'a>> for Statement<'a> {
    fn from(expr: VariableDeclarationStmt<'a>) -> Self {
        Statement::Variable(expr)
    }
}
