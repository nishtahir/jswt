mod class;
mod functions;
mod variables;

use self::class::ClassDeclarationGlobalContext;
use self::functions::FunctionDeclarationGlobalContext;
use self::variables::VariableDeclarationGlobalContext;
use crate::SemanticError;
use jswt_ast::*;
use jswt_symbols::{SemanticEnvironment, SymbolTable};

/// Global Semantic Resolver to resolve global variables and functions
/// This should usually be the first pass of the semantic analysis phase
#[derive(Debug)]
pub struct GlobalSemanticResolver<'a> {
    environment: &'a mut SemanticEnvironment,
    errors: Vec<SemanticError>,
}

impl<'a> GlobalSemanticResolver<'a> {
    pub fn new(environment: &'a mut SemanticEnvironment) -> Self {
        Self {
            environment,
            errors: vec![],
        }
    }

    /// Run the global semantic analysis pass
    pub fn resolve(&mut self, ast: &Ast) {
        self.environment.push_global_symbol_scope();
        self.visit_program(&ast.program);
        // We should have only the global scope left
        // at the end of the pass
        debug_assert!(self.environment.symbol_scope_depth() == 1);
    }

    pub fn errors(&mut self) -> &mut Vec<SemanticError> {
        &mut self.errors
    }
}

impl<'a> Visitor for GlobalSemanticResolver<'a> {
    // fn visit_file(&mut self, node: &File) {
    //     self.symbols.push_scope(node.span());
    //     visit::walk_file(self, node);
    //     self.symbols.pop_scope();
    // }

    fn visit_function_declaration_element(&mut self, node: &FunctionDeclarationElement) {
        let mut ctx = FunctionDeclarationGlobalContext::new(self);
        ctx.visit_function_declaration_element(node);
    }

    fn visit_variable_declaration_element(&mut self, node: &VariableDeclarationElement) {
        let mut ctx = VariableDeclarationGlobalContext::new(self);
        ctx.visit_variable_declaration_element(node);
    }

    fn visit_class_declaration_element(&mut self, node: &ClassDeclarationElement) {
        let mut ctx = ClassDeclarationGlobalContext::new(self, node);
        ctx.visit_class_declaration_element(node);
    }
}
