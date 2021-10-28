use self::statement::Statement;

pub mod expression;
pub mod function;
pub mod ident;
pub mod literal;
pub mod node;
pub mod operator;
pub mod span;
pub mod statement;
pub mod types;

#[derive(Debug)]

pub struct Ast<'a> {
    pub statements: Vec<Statement<'a>>,
}
