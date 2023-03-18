mod class;
mod functions;
mod variables;

use self::class::ClassDeclarationGlobalContext;
use self::functions::FunctionDeclarationGlobalContext;
use self::variables::VariableDeclarationGlobalContext;
use crate::SemanticError;
use jswt_ast::{visit::Visitor, *};
use jswt_symbols::BindingsTable;
use jswt_symbols::ScopedSymbolTable;

/// Global Semantic Resolver to resolve global variables and functions
/// This should usually be the first pass of the semantic analysis phase
#[derive(Debug)]
pub struct GlobalSemanticResolver<'a> {
    bindings: &'a mut BindingsTable,
    symbols: &'a mut ScopedSymbolTable,
    errors: Vec<SemanticError>,
}

impl<'a> GlobalSemanticResolver<'a> {
    pub fn new(bindings: &'a mut BindingsTable, symbols: &'a mut ScopedSymbolTable) -> Self {
        Self {
            bindings,
            symbols,
            errors: vec![],
        }
    }

    /// Run the global semantic analysis pass
    pub fn resolve(&mut self, ast: &Ast) {
        self.symbols.push_global_scope();
        self.visit_program(&ast.program);
        // We should have only the global scope left
        // at the end of the pass
        debug_assert!(self.symbols.depth() == 1);
    }

    pub fn errors(&mut self) -> &mut Vec<SemanticError> {
        &mut self.errors
    }
}

impl<'a> Visitor for GlobalSemanticResolver<'a> {
    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        let mut ctx = FunctionDeclarationGlobalContext::new(self);
        ctx.visit_function_declaration(node);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        let mut ctx = VariableDeclarationGlobalContext::new(self);
        ctx.visit_variable_statement(node);
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let mut ctx = ClassDeclarationGlobalContext::new(self, node);
        ctx.visit_class_declaration(node);
    }
}
