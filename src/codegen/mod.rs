use crate::{
    ast::program::*,
    ast::{visitor::Visitor, Ast},
    common::SymbolTable,
    wast::{
        Export, Function, FunctionExport, FunctionType, Instruction, Module, Type, ValueType,
        WastSymbol,
    },
};

impl Default for CodeGenerator {
    fn default() -> Self {
        Self {
            module: Default::default(),
            scopes: Default::default(),
            symbols: SymbolTable::new(vec![]),
        }
    }
}

#[derive(Debug)]
pub struct CodeGenerator {
    module: Module,
    scopes: Vec<Function>,
    symbols: SymbolTable<WastSymbol>,
}

impl CodeGenerator {
    pub fn generate_module(&mut self, ast: &Ast) -> &Module {
        self.visit_program(&ast.program);
        &self.module
    }

    fn push_type(&mut self, new_ty: Type) -> usize {
        for (i, ty) in self.module.types.iter().enumerate() {
            if *ty == new_ty {
                return i;
            }
        }
        self.module.types.push(new_ty);
        self.module.types.len() - 1
    }

    fn push_function(&mut self, func: Function) -> usize {
        self.module.functions.push(func);
        self.module.functions.len() - 1
    }

    fn push_export(&mut self, export: Export) -> usize {
        self.module.exports.push(export);
        self.module.functions.len() - 1
    }

    fn push_instruction(&mut self, inst: Instruction) {
        let function = self.scopes.last_mut().unwrap();
        function.instructions.push(inst);
    }
}

impl Visitor for CodeGenerator {
    fn visit_program(&mut self, node: &Program) {
        self.visit_source_elements(&node.source_elements);
    }

    fn visit_source_elements(&mut self, node: &SourceElements) {
        for element in &node.source_elements {
            self.visit_source_element(element);
        }
    }

    fn visit_source_element(&mut self, node: &SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(function) => {
                self.visit_function_declaration(function)
            }
            SourceElement::Statement(statement) => self.visit_statement_element(statement),
        }
    }

    fn visit_statement_element(&mut self, node: &StatementElement) {
        match node {
            StatementElement::Block(block) => self.visit_block_statement(block),
            StatementElement::Empty(empty) => self.visit_empty_statement(empty),
            StatementElement::Return(ret) => self.visit_return_statement(ret),
            StatementElement::Variable(variable) => self.visit_variable_statement(variable),
        }
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.visit_statement_list(&node.statements);
    }

    fn visit_empty_statement(&mut self, node: &EmptyStatement) {
        // No-op
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        self.visit_single_expression(&node.expression);
        self.push_instruction(Instruction::Return);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        // Emit assignment
        self.visit_assignable_element(&node.target);
        self.visit_single_expression(&node.expression);
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        // Push the scope for the function body
        self.symbols.push_scope();
        let mut type_params = vec![];
        // Push Symbols for Params
        for (i, arg) in node.params.parameters.iter().enumerate() {
            // Add to symbol table
            self.symbols.define(arg.ident.value, WastSymbol::Param(i));
            // Todo, convert to ValueType
            type_params.push(ValueType::I32)
        }

        // Resolve return Value
        let ty = FunctionType {
            params: type_params,
            ret: node.returns.as_ref().map(|_| ValueType::I32),
        };
        // Add type definition to type index
        let type_idx = self.push_type(Type::Function(ty));
        let function = Function {
            type_idx,
            ..Default::default()
        };
        // Make function the current scope context for emitting instructions
        self.scopes.push(function);

        // Generate instructions for the current scope context
        self.visit_function_body(&node.body);

        // Function generation is done. Pop it from the current scope context
        // and commit it to the module
        let f = self.scopes.pop().unwrap();
        let function_idx = self.push_function(f);

        // Generate export descriptor if the function is marked for export
        if node.decorators.export {
            let desc = FunctionExport {
                function_idx,
                name: node.ident.value,
            };
            self.push_export(Export::Function(desc));
        }
    }

    fn visit_function_body(&mut self, node: &FunctionBody) {
        self.visit_source_elements(&node.source_elements);
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) {
        todo!()
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) {
        match node {
            SingleExpression::Literal(lit) => self.visit_literal(lit),
            SingleExpression::Multiplicative(exp) => self.visit_binary_expression(exp),
            SingleExpression::Additive(exp) => self.visit_binary_expression(exp),
            SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
        }
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        self.visit_single_expression(&node.left);
        self.visit_single_expression(&node.right);

        // TODO - type check before pushing op
        match node.op {
            BinaryOperator::Plus(_) => self.push_instruction(Instruction::I32Add),
            BinaryOperator::Minus(_) => todo!(),
            BinaryOperator::Star(_) => todo!(),
            BinaryOperator::Slash(_) => todo!(),
        }
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        todo!()
    }

    fn visit_literal(&mut self, node: &Literal) {
        match node {
            Literal::String(_) => todo!(),
            Literal::Number(lit) => self.push_instruction(Instruction::I32Const(lit.value)),
            Literal::Boolean(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{Parser, Tokenizer};

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_empty_ast_generates_empty_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let module = generator.generate_module(&ast);

        assert_eq!(
            module,
            &Module {
                exports: vec![],
                types: vec![],
                functions: vec![]
            }
        )
    }

    #[test]
    fn test_ast_with_empty_function_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() {}");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let module = generator.generate_module(&ast);

        assert_eq!(
            module,
            &Module {
                exports: vec![],

                types: vec![Type::Function(FunctionType {
                    params: vec![],
                    ret: None
                })],
                functions: vec![Function::default()]
            }
        )
    }

    #[test]
    fn test_ast_with_empty_function_with_params_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(a: i32) {}");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let module = generator.generate_module(&ast);

        assert_eq!(
            module,
            &Module {
                exports: vec![],

                types: vec![Type::Function(FunctionType {
                    params: vec![ValueType::I32],
                    ret: None
                })],
                functions: vec![Function::default()]
            }
        )
    }
    #[test]
    fn test_ast_with_empty_function_with_params_and_return_value_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(a: i32): i32 {}");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let module = generator.generate_module(&ast);

        assert_eq!(
            module,
            &Module {
                exports: vec![],

                types: vec![Type::Function(FunctionType {
                    params: vec![ValueType::I32],
                    ret: Some(ValueType::I32)
                })],
                functions: vec![Function::default()]
            }
        )
    }

    #[test]
    fn test_ast_with_function_containing_return_expression_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { return 1 + 2; }");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let module = generator.generate_module(&ast);

        assert_eq!(
            module,
            &Module {
                exports: vec![],

                types: vec![Type::Function(FunctionType {
                    params: vec![],
                    ret: None
                })],
                functions: vec![Function {
                    type_idx: 0,
                    instructions: vec![
                        Instruction::I32Const(1),
                        Instruction::I32Const(2),
                        Instruction::I32Add,
                        Instruction::Return,
                    ]
                }]
            }
        )
    }
}
