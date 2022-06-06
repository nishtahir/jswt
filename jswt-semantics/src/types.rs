use crate::{SemanticError, SymbolTable};
use jswt_ast::mut_visit::*;
use jswt_ast::*;
use jswt_common::{Type, Typeable};
use jswt_symbols::{BindingsTable, Symbol};

pub struct TypeChecker<'a> {
    pub symbols: &'a mut SymbolTable,
    pub bindings: &'a mut BindingsTable,
    pub errors: Vec<SemanticError>,
    binding_context: Option<String>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(symbols: &'a mut SymbolTable, bindings: &'a mut BindingsTable) -> Self {
        Self {
            symbols,
            bindings,
            errors: vec![],
            binding_context: None,
        }
    }

    pub fn resolve(&mut self, ast: &mut Ast) {
        debug_assert!(self.symbols.depth() == 1);
        self.visit_program(&mut ast.program);
        debug_assert!(self.symbols.depth() == 1);
    }
}

impl<'a> MutVisitor for TypeChecker<'a> {
    fn visit_class_declaration(&mut self, node: &mut ClassDeclarationElement) {
        let name = node.ident.value.clone();
        self.binding_context = Some(name.to_string());
        walk_class_declaration(self, node);
        self.binding_context = None;
    }

    fn visit_function_declaration(&mut self, node: &mut FunctionDeclarationElement) {
        // Push a new local scope for the function
        // Scope definition should have been defined during the global pass
        self.symbols.push_scope();

        // Add function parameters as variables in scope
        for param in node.params.parameters.iter() {
            // Resolve Type from Type Annotation
            let param_name = &param.ident.value;
            self.symbols.define(
                param_name.clone(),
                Symbol::ty(param.type_annotation.ty.clone()),
            );
        }

        walk_function_declaration(self, node);
        self.symbols.pop_scope();
    }

    fn visit_class_constructor_declaration(&mut self, node: &mut ClassConstructorElement) {
        self.symbols.push_scope();
        // Add function parameters as variables in scope
        for param in node.params.parameters.iter() {
            // Resolve Type from Type Annotation
            let param_name = &param.ident.value;
            self.symbols.define(
                param_name.clone(),
                Symbol::ty(param.type_annotation.ty.clone()),
            );
        }

        walk_class_constructor_declaration(self, node);
        self.symbols.pop_scope();
    }

    fn visit_class_method_declaration(&mut self, node: &mut ClassMethodElement) {
        self.symbols.push_scope();
        // Add function parameters as variables in scope
        for param in node.params.parameters.iter() {
            // Resolve Type from Type Annotation
            let param_name = &param.ident.value;
            self.symbols.define(
                param_name.clone(),
                Symbol::ty(param.type_annotation.ty.clone()),
            );
        }

        walk_class_method_declaration(self, node);
        self.symbols.pop_scope();
    }

    fn visit_block_statement(&mut self, node: &mut BlockStatement) {
        self.symbols.push_scope();
        walk_block_statement(self, node);
        self.symbols.pop_scope();
    }

    fn visit_variable_statement(&mut self, node: &mut VariableStatement) {
        walk_variable_statement(self, node);
        let expression_type = node.expression.ty();

        // We are in a local scope
        let name = match &node.target {
            AssignableElement::Identifier(ident) => &ident.value,
        };

        let declared_type = node.type_annotation.as_ref().map(|t| t.ty.clone());
        if let Some(ty) = declared_type {
            if ty != expression_type {
                todo!(
                    "error: declared {},  but found {}",
                    ty.to_string(),
                    expression_type.to_string()
                );
            }
        }

        self.symbols
            .define(name.clone(), Symbol::ty(expression_type));
    }

    fn visit_identifier_expression(&mut self, node: &mut IdentifierExpression) {
        let name = &node.ident.value;
        if let Some(sym) = self.symbols.lookup(name) {
            match sym {
                Symbol::Type(sig) => node.ty = sig.ty.clone(),
                // TODO: Technically incorrect
                Symbol::Function(sig) => node.ty = sig.returns.clone(),
                Symbol::Unknown => todo!(),
            }
        }
    }

    fn visit_assignment_expression(&mut self, node: &mut BinaryExpression) {
        walk_assignment_expression(self, node);
        if let SingleExpression::Identifier(ident) = &mut *node.left {
            ident.ty = node.right.ty();
        };
    }

    fn visit_argument_expression(&mut self, node: &mut ArgumentsExpression) {
        walk_argument_expression(self, node);
        if let Some(target) = node.ident.as_identifier() {
            let name = &target.ident.value;
            if let Some(Symbol::Function(sig)) = self.symbols.lookup(name) {
                node.ty = sig.returns.clone();
            }
        }
    }

    fn visit_binary_expression(&mut self, node: &mut BinaryExpression) {
        walk_binary_expression(self, node);
        // TODO - evaluate this corectly
        // let lhs = node.left.ty();
        // let rhs = node.right.ty();
        node.ty = node.left.ty();
    }

    fn visit_this_expression(&mut self, node: &mut ThisExpression) {
        node.ty = Type::Binding("ptr".into());
    }

    fn visit_member_dot(&mut self, node: &mut MemberDotExpression) {
        walk_member_dot(self, node);

        if let SingleExpression::This(_) = &*node.target {
            let binding = self.binding_context.as_ref().unwrap();
            let binding = self.bindings.lookup(binding).unwrap();
            if let Some(target) = node.expression.as_identifier() {
                if let Some(field) = binding.field(&target.ident.value) {
                    node.ty = field.ty.clone();
                }
            }
        }
    }

    fn visit_literal(&mut self, node: &mut Literal) {
        match node {
            Literal::Array(_) => todo!(),
            Literal::String(s) => s.ty = Type::Binding("string".into()),
            Literal::Integer(i) => i.ty = Type::Binding("i32".into()),
            Literal::Float(f) => f.ty = Type::Binding("f32".into()),
            Literal::Boolean(b) => b.ty = Type::Binding("boolean".into()),
        }
    }
}
