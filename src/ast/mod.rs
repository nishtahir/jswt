pub mod ident;
pub mod program;
pub mod span;

use self::program::Program;

#[derive(Debug)]
pub struct Ast<'a> {
    pub program: Program<'a>,
}
