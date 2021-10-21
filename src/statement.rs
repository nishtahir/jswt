use crate::{expression::Expr, node::Node};

#[derive(Debug, PartialEq)]

pub enum Statement<'a> {
    Print(PrintStmt<'a>),
    Variable(VariableDeclarationStmt<'a>),
    Block(BlockStmt<'a>),
    Function(FunctionStmt<'a>),
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

    pub fn function(ident: Node<'a>, body: Statement<'a>, returns: Option<Node<'a>>) -> Self {
        Statement::Function(FunctionStmt {
            ident,
            body: Box::new(body),
            returns,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionStmt<'a> {
    ident: Node<'a>,
    body: Box<Statement<'a>>,
    returns: Option<Node<'a>>,
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
