mod error;
mod globals;
mod resolver;
mod symbol;

use jswt_ast::Ast;

pub use error::*;
use globals::*;
use resolver::*;
pub use symbol::*;

pub struct SemanticAnalyzer;

impl SemanticAnalyzer {
    pub fn analyze(ast: &Ast) -> Vec<SemanticError> {
        let mut global_resolver = GlobalResolver::default();
        global_resolver.resolve(ast);

        let global_symbols = global_resolver.symbols;
        let mut resolver = Resolver::new(global_symbols);
        resolver.resolve(ast);

        let mut errors = vec![];
        errors.append(&mut global_resolver.errors);
        errors.append(&mut resolver.errors);

        errors
    }
}
