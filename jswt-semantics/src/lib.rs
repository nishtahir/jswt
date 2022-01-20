mod bindings;
mod desugaring;
mod error;
mod globals;
mod resolver;
mod symbols;

use bindings::BindingsTable;
use desugaring::AstDesugaring;
pub use error::*;
use globals::*;
use resolver::*;

use jswt_ast::Ast;
use symbols::SymbolTable;

pub struct SemanticAnalyzer;

impl SemanticAnalyzer {
    pub fn analyze(ast: &mut Ast) -> Vec<SemanticError> {
        let mut symbol_table = SymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut errors = vec![];

        let mut desugaring = AstDesugaring::default();
        desugaring.desugar(ast);

        // This is the first semantic pass
        // let mut global_resolver = GlobalResolver::new(&mut symbol_table, &mut bindings_table);
        // global_resolver.resolve(ast);
        // errors.append(&mut global_resolver.errors());

        // // This is the second semantic pass
        // let mut resolver = Resolver::new(&mut symbol_table, &mut bindings_table);
        // resolver.resolve(ast);
        // errors.append(&mut resolver.errors);

        errors
    }
}
