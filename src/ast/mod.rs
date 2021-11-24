pub mod ident;
pub mod program;
pub mod span;
pub mod visitor;

use self::program::Program;

#[derive(Debug)]

pub struct Ast {
    pub program: Program,
}

impl Ast {
    pub fn new(program: Program) -> Self {
        Self { program }
    }
}
