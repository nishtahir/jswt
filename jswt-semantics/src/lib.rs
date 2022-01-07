mod bindings;
mod desugaring;
mod error;
mod globals;
mod resolver;
mod symbol;
mod symbols;

use desugaring::AstDesugaring;
pub use error::*;
use globals::*;
use resolver::*;
pub use symbol::*;

use jswt_ast::Ast;

pub struct SemanticAnalyzer;

impl SemanticAnalyzer {
    pub fn analyze(ast: &mut Ast) -> Vec<SemanticError> {
        // Desugaring pass
        let mut desugaring = AstDesugaring::default();
        desugaring.desugar(ast);

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
