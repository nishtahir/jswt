mod symbols;

use std::borrow::Borrow;
use symbols::{WastSymbol, WastSymbolTable};

use jswt_ast::*;
use jswt_wast::*;

#[derive(Debug)]
pub struct CodeGenerator {
    module: Module,
    /// The architecture here assumes that this is an instruction scope stack
    /// any lexical scope that wishes to recieve an emitted set of instructions
    /// should push a new scope to the stack and pop the scope before pushing their own
    /// instruction to the stack
    scopes: Vec<InstructionScope>,
    symbols: WastSymbolTable,
    label_counter: usize,
}

#[derive(Debug)]
struct InstructionScope {
    instructions: Vec<Instruction>,
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self {
            module: Default::default(),
            scopes: Default::default(),
            symbols: WastSymbolTable::new(),
            label_counter: 0,
        }
    }
}

impl CodeGenerator {
    pub fn generate_module(&mut self, ast: &Ast) -> &Module {
        // TODO - we should be accepting builtins externally from the env
        // This is a stop gap so tests don't break
        self.visit_program(&ast.program);
        &self.module
    }

    fn push_import(&mut self, import: Import) -> usize {
        self.module.imports.push(import);
        self.module.imports.len() - 1
    }

    fn push_global(&mut self, global: GlobalType) -> usize {
        self.module.globals.push(global);
        self.module.globals.len() - 1
    }

    fn push_type(&mut self, new_ty: FunctionType) -> usize {
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
        let scope = self.scopes.last_mut().unwrap();
        scope.instructions.push(inst);
    }

    /// Adds a new instruction scope context to the stack
    fn push_instruction_scope(&mut self) {
        self.scopes.push(InstructionScope {
            instructions: vec![],
        });
    }

    /// Pops an Instruction scope from the stack
    fn pop_instruction_scope(&mut self) -> Option<InstructionScope> {
        self.scopes.pop()
    }
}

impl ProgramVisitor<()> for CodeGenerator {
    fn visit_program(&mut self, node: &Program) {
        // Push global scope
        self.symbols.push_scope();
        for file in &node.files {
            self.visit_file(file)
        }
        // Pop global scope from stack
        debug_assert!(self.symbols.depth() == 1);
        self.symbols.pop_scope();
    }

    fn visit_file(&mut self, node: &File) {
        self.visit_source_elements(&node.source_elements);
    }

    fn visit_source_elements(&mut self, node: &SourceElements) {
        for element in &node.source_elements {
            self.visit_source_element(element);
        }
    }

    fn visit_source_element(&mut self, node: &SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(elem) => self.visit_function_declaration(elem),
            SourceElement::ClassDeclaration(elem) => self.visit_class_declaration(elem),
            SourceElement::VariableDeclaration(elem) => self.visit_variable_declaration(elem),
            SourceElement::ImportDeclaration(elem) => self.visit_import_declaration(elem),
        }
    }
}

impl StatementVisitor<()> for CodeGenerator {
    fn visit_statement_element(&mut self, node: &StatementElement) {
        match node {
            StatementElement::Block(stmt) => self.visit_block_statement(stmt),
            StatementElement::Empty(stmt) => self.visit_empty_statement(stmt),
            StatementElement::Return(stmt) => self.visit_return_statement(stmt),
            StatementElement::Variable(stmt) => self.visit_variable_statement(stmt),
            StatementElement::Expression(stmt) => self.visit_expression_statement(stmt),
            StatementElement::If(stmt) => self.visit_if_statement(stmt),
            StatementElement::Iteration(stmt) => self.visit_iteration_statement(stmt),
        }
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.visit_statement_list(&node.statements);
    }

    fn visit_empty_statement(&mut self, _: &EmptyStatement) {
        // No-op
    }

    fn visit_if_statement(&mut self, node: &IfStatement) {
        let cond = self.visit_single_expression(&node.condition);

        // Handle the consequence instructions
        self.push_instruction_scope();
        self.visit_statement_element(&node.consequence);
        let cons = self.pop_instruction_scope().unwrap();

        // Handle the alternative instructions
        self.push_instruction_scope();
        if let Some(alternative) = node.alternative.borrow() {
            self.visit_statement_element(alternative);
        }
        let alt = self.pop_instruction_scope().unwrap();

        self.push_instruction(Instruction::If(
            Box::new(cond),
            cons.instructions,
            alt.instructions,
        ));
    }

    fn visit_iteration_statement(&mut self, node: &IterationStatement) {
        match node {
            IterationStatement::While(elem) => self.visit_while_iteration_element(elem),
            IterationStatement::For(_) => todo!(),
        }
    }

    fn visit_while_iteration_element(&mut self, node: &WhileIterationElement) {
        let loop_label = self.label_counter;
        self.label_counter += 1;

        self.push_instruction_scope();

        // First push the expression result onto the stack
        let cond = self.visit_single_expression(&node.expression);

        // Push an if scope for branching
        self.push_instruction_scope();
        // Add the statements to the scope
        self.visit_block_statement(&node.block);

        // Add the branch back to the top of the loop to test
        self.push_instruction(Instruction::BrLoop(loop_label));

        let if_scope = self.pop_instruction_scope().unwrap();
        self.push_instruction(Instruction::If(
            Box::new(cond),
            if_scope.instructions,
            vec![],
        ));

        let loop_scope = self.pop_instruction_scope().unwrap();
        self.push_instruction(Instruction::Loop(loop_label, loop_scope.instructions));
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        let exp = self.visit_single_expression(&node.expression);
        self.push_instruction(Instruction::Return(Box::new(exp)));
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        // This should be assignment local.set, global.set
        let target = self.visit_assignable_element(&node.target);
        let exp = self.visit_single_expression(&node.expression);
        match target {
            Instruction::GlobalSet(name, _) => {
                self.push_global(GlobalType {
                    name,
                    ty: ValueType::I32,
                    mutable: true, // TODO - check mutability
                    initializer: exp,
                });
            }
            Instruction::LocalSet(name, _) => {
                let exp = Instruction::LocalSet(name, Box::new(exp));
                self.push_instruction(exp)
            }
            _ => self.push_instruction(exp),
        }
    }

    fn visit_expression_statement(&mut self, node: &ExpressionStatement) {
        let isr = self.visit_single_expression(&node.expression);
        self.push_instruction(isr);
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        let function_name = node.ident.value.clone();

        // Push the scope for the function body
        self.symbols.push_scope();
        let mut type_params = vec![];
        // Push Symbols for Params. We need this in case the scope
        // needs to declare synthetic local variables
        for (index, arg) in node.params.parameters.iter().enumerate() {
            // Add to symbol table
            self.symbols.define(
                arg.ident.value.clone(),
                WastSymbol::Param(index, ValueType::I32),
            );
            // Todo, convert to ValueType
            type_params.push((arg.ident.value.clone(), ValueType::I32))
        }

        // Resolve return Value
        let ty = FunctionType {
            params: type_params,
            ret: node.returns.as_ref().map(|_| ValueType::I32),
        };
        // Add type definition to type index
        let type_idx = self.push_type(ty);

        // Push a new Instruction scope to hold emitted instructions
        self.push_instruction_scope();

        // Resolve annotations
        let mut has_inlined_body = false;
        let mut is_predefined_function = false;
        for annotation in &node.decorators.annotations {
            match annotation.name.value.as_ref() {
                // The "wast" annotation allows the developer to emit
                // WAST instructions directily into the instruction scope
                // of the function.
                "wast" => match &annotation.expr {
                    Some(SingleExpression::Literal(Literal::String(string_lit))) => {
                        has_inlined_body = true;
                        self.push_instruction(Instruction::RawWast(string_lit.value.into()));
                    }
                    _ => todo!(),
                },
                "native" => match &annotation.expr {
                    Some(SingleExpression::Literal(Literal::String(lit))) => {
                        has_inlined_body = true;
                        is_predefined_function = true;
                        self.push_import(Import::Function(FunctionImport {
                            name: function_name.clone(),
                            type_idx,
                            module: lit.value.into(),
                        }));
                    }
                    _ => todo!(),
                },
                "inline" => {}
                _ => {}
            }
        }

        let mut instructions = vec![];
        // Generate instructions for the current scope context
        // If we haven't already inlined a function body
        if !has_inlined_body {
            self.visit_block_statement(&node.body);
            // Function generation is done. Pop the current instructions scope
            // and commit it to the module
            let scope = self.pop_instruction_scope().unwrap();

            instructions.push(Instruction::Block(0, scope.instructions));
            // Add synthetic return value
            // This is to make dealing with branching returns easier to manage
            // We're using a keyword here to prevent users from accidentally shadowing the value
            if node.returns.is_some() {
                self.symbols
                    .define("return", WastSymbol::Local(ValueType::I32));

                // We're pushing the synthetic return to the end of the function
                instructions.push(Instruction::SynthReturn);
            }
        } else {
            // Just use the scope as is we assume that the user knows what
            // they are doing.
            instructions = self.pop_instruction_scope().unwrap().instructions;
        }

        // Push our locals to the instructions
        for (name, sym) in self.symbols.symbols_in_current_scope() {
            if let WastSymbol::Local(ty) = sym {
                instructions.insert(0, Instruction::Local(name.clone(), *ty))
            }
        }

        if !is_predefined_function {
            let function = Function {
                name: function_name.clone(),
                type_idx,
                instructions,
            };
            let function_idx = self.push_function(function);

            // Generate export descriptor if the function is marked for export
            if node.decorators.export {
                let desc = FunctionExport {
                    function_idx,
                    name: function_name.clone(),
                };
                self.push_export(Export::Function(desc));
            }
        }

        // Pop the current function scope from the symbol table
        self.symbols.pop_scope();
    }

    // fn visit_function_body(&mut self, node: &FunctionBody) {
    //     self.visit_source_elements(&node.source_elements);
    // }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        self.visit_class_body(&node.body);
    }

    fn visit_class_body(&mut self, node: &ClassBody) {
        for element in &node.class_elements {
            match element {
                ClassElement::Constructor(elem) => self.visit_class_constructor_declaration(elem),
                ClassElement::Method(elem) => self.visit_class_method_declaration(elem),
                ClassElement::Field(elem) => self.visit_class_field_declaration(elem),
            }
        }
    }

    fn visit_class_constructor_declaration(&mut self, _: &ClassConstructorElement) {
        unreachable!("This should have been desugared out of the AST")
    }

    fn visit_class_method_declaration(&mut self, _: &ClassMethodElement) {
        unreachable!("This should have been desugared out of the AST")
    }

    fn visit_class_field_declaration(&mut self, _: &ClassFieldElement) {
        unreachable!("This should have been desugared out of the AST")
    }

    fn visit_variable_declaration(&mut self, node: &VariableDeclarationElement) -> () {
        // Declarations are always in the global scope
        let name = node.name.value.clone();
        let exp = self.visit_single_expression(&node.expression);

        // Add the variable to the symbol table
        self.symbols
            .define(name.clone(), WastSymbol::Global(ValueType::I32));

        // Add the variable to the list of global variables
        // TODO: This needs to be namespaced to avoid collisions
        self.push_global(GlobalType {
            name,
            ty: ValueType::I32,
            mutable: !node.modifier.is_const(),
            initializer: exp,
        });
    }

    fn visit_import_declaration(&mut self, _node: &ImportDeclarationElement) {
        // No-op
    }
}

impl ExpressionVisitor<Instruction> for CodeGenerator {
    fn visit_single_expression(&mut self, node: &SingleExpression) -> Instruction {
        match node {
            SingleExpression::Additive(exp)
            | SingleExpression::Multiplicative(exp)
            | SingleExpression::Equality(exp)
            | SingleExpression::Bitwise(exp)
            | SingleExpression::Relational(exp) => self.visit_binary_expression(exp),
            SingleExpression::Arguments(exp) => self.visit_argument_expression(exp),
            SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
            SingleExpression::Literal(lit) => self.visit_literal(lit),
            SingleExpression::Assignment(exp) => self.visit_assignment_expression(exp),
            SingleExpression::Unary(exp) => self.visit_unary_expression(exp),
            SingleExpression::MemberIndex(exp) => self.visit_member_index(exp),
            SingleExpression::This(exp) => self.visit_this_expression(exp),
            SingleExpression::MemberDot(exp) => self.visit_member_dot(exp),
            SingleExpression::New(exp) => self.visit_new(exp),
        }
    }

    fn visit_assignable_element(&mut self, elem: &AssignableElement) -> Instruction {
        // Figure out the target for an assignment
        // We assume that this is the target for
        // the current instruction scope
        match elem {
            AssignableElement::Identifier(ident) => {
                let name = &ident.value;
                // Check if this element has been defined
                if self.symbols.lookup(name.clone()).is_none() {
                    if self.symbols.depth() == 1 {
                        self.symbols
                            .define(name.clone(), WastSymbol::Global(ValueType::I32));
                        return Instruction::GlobalSet(name.clone(), Box::new(Instruction::Noop));
                    } else {
                        self.symbols
                            .define(name.clone(), WastSymbol::Local(ValueType::I32));

                        return Instruction::LocalSet(name.clone(), Box::new(Instruction::Noop));
                    }
                }
                unreachable!()
            }
        }
    }

    fn visit_member_dot(&mut self, _: &MemberDotExpression) -> Instruction {
        todo!()
    }

    fn visit_member_index(&mut self, node: &MemberIndexExpression) -> Instruction {
        let container = self.visit_single_expression(&node.target);
        let index = self.visit_single_expression(&node.index);
        Instruction::Call("arrayAt".into(), vec![container, index])
    }

    fn visit_new(&mut self, _: &NewExpression) -> Instruction {
        todo!()
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) -> Instruction {
        let target = &node.ident.value;
        if self.symbols.lookup_global(target.clone()).is_some() {
            Instruction::GlobalGet(target.clone())
        } else {
            Instruction::LocalGet(target.clone())
        }
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> Instruction {
        if let SingleExpression::Identifier(ident_exp) = node.ident.borrow() {
            // Push a new instruction scope for the function call
            let instructions = node
                .arguments
                .arguments
                .iter()
                .map(|exp| self.visit_single_expression(exp))
                .collect();

            return Instruction::Call(ident_exp.ident.value.clone(), instructions);
        }

        // Other targets for function calls.
        todo!()
    }

    fn visit_unary_expression(&mut self, node: &UnaryExpression) -> Instruction {
        let exp = self.visit_single_expression(&node.expr);
        match node.op {
            UnaryOperator::Plus(_) => todo!(),
            UnaryOperator::Minus(_) => {
                Instruction::I32Sub(Box::new(Instruction::I32Const(0)), Box::new(exp))
            }
            UnaryOperator::Not(_) => {
                Instruction::I32Xor(Box::new(exp), Box::new(Instruction::I32Const(-1)))
            }
            UnaryOperator::PostIncrement(_) => {
                // This is actually incorrect. This is the behavior
                // of a prefix increment. We should be evaulating this into
                // 2 instructions. The assingment of the original value and the increment
                Instruction::I32Add(Box::new(exp), Box::new(Instruction::I32Const(1)))
            }
            UnaryOperator::PostDecrement(_) => {
                // This is actually incorrect. This is the behavior
                // of a prefix increment. We should be evaulating this into
                // 2 instructions. The assingment of the original value and the decrement
                Instruction::I32Sub(Box::new(exp), Box::new(Instruction::I32Const(1)))
            }
        }
    }

    fn visit_assignment_expression(&mut self, node: &AssignmentExpression) -> Instruction {
        let rhs = self.visit_single_expression(node.expression.borrow());

        match node.target.borrow() {
            SingleExpression::Identifier(ident_exp) => {
                let name = &ident_exp.ident.value;
                // figure out the scope of the variable
                let isr = if self.symbols.lookup_global(name.clone()).is_some() {
                    Instruction::GlobalSet
                } else {
                    Instruction::LocalSet
                };
                isr(name.clone(), Box::new(rhs))
            }
            SingleExpression::MemberIndex(exp) => {
                let index_ptr = self.visit_member_index(exp);
                Instruction::I32Store(Box::new(index_ptr), Box::new(rhs))
            }
            SingleExpression::Unary(_) => todo!(),
            SingleExpression::Assignment(_) => todo!(),
            SingleExpression::New(_) => todo!(),
            SingleExpression::Arguments(_) => todo!(),
            SingleExpression::Multiplicative(_) => todo!(),
            SingleExpression::Bitwise(_) => todo!(),
            SingleExpression::Additive(_) => todo!(),
            SingleExpression::Equality(_) => todo!(),
            SingleExpression::Relational(_) => todo!(),
            SingleExpression::MemberDot(_) => todo!(),
            SingleExpression::This(_) => todo!(),
            SingleExpression::Literal(_) => todo!(),
        }
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) -> Instruction {
        let lhs = self.visit_single_expression(&node.left);
        let rhs = self.visit_single_expression(&node.right);

        // TODO - type check before pushing op
        match node.op {
            BinaryOperator::Plus(_) => Instruction::I32Add(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Minus(_) => Instruction::I32Sub(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Mult(_) => Instruction::I32Mul(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Equal(_) => Instruction::I32Eq(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::NotEqual(_) => Instruction::I32Neq(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Div(_) => Instruction::I32Div(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::And(_) => Instruction::I32And(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Or(_) => Instruction::I32Or(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Greater(_) => Instruction::I32Gt(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::GreaterEqual(_) => Instruction::I32Ge(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Less(_) => Instruction::I32Lt(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::LessEqual(_) => Instruction::I32Le(Box::new(lhs), Box::new(rhs)),
            BinaryOperator::Assign(_) => todo!(),
        }
    }

    fn visit_this_expression(&mut self, _: &ThisExpression) -> Instruction {
        todo!()
    }

    fn visit_literal(&mut self, node: &Literal) -> Instruction {
        match node {
            Literal::String(_) => todo!(),
            Literal::Integer(lit) => Instruction::I32Const(lit.value),
            Literal::Float(_) => todo!(),
            Literal::Boolean(lit) => match lit.value {
                // Boolean values in WebAssembly are represented as values of type i32. In a boolean context,
                // such as a br_if condition, any non-zero value is interpreted as true and 0 is interpreted as false.
                true => Instruction::I32Const(1),
                false => Instruction::I32Const(0),
            },
            Literal::Array(lit) => {
                // Synthetic variable to hold the array pointer
                let array_pointer = self.symbols.define_synthetic_local(ValueType::I32);
                let mut instructions = vec![Instruction::LocalSet(
                    array_pointer.clone(),
                    Box::new(Instruction::Call(
                        "arrayNew".into(),
                        vec![Instruction::I32Const(4)], // Size of i32 in bytes
                    )),
                )];

                for element in &lit.elements {
                    // instructions.push(Instruction::I32Store());
                    let value = self.visit_single_expression(element);
                    instructions.push(Instruction::I32Store(
                        Box::new(Instruction::Call(
                            "arrayPush".into(),
                            vec![Instruction::LocalGet(array_pointer.clone())], // Size of i32 in bytes
                        )),
                        Box::new(value),
                    ));
                }

                // Return the array pointer as the result of the expression
                instructions.push(Instruction::LocalGet(array_pointer.clone()));
                Instruction::Complex(instructions)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    #[ignore]
    fn test_empty_ast_generates_empty_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_empty_ast_generates_empty_module", "");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    #[ignore]
    fn test_ast_with_empty_function_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_ast_with_empty_function_generates_module",
            "function test() {}",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    #[ignore]
    fn test_ast_with_empty_function_with_params_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_ast_with_empty_function_with_params_generates_module",
            "function test(a: i32) {}",
        );
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }
    #[test]
    #[ignore]
    fn test_ast_with_empty_function_with_params_and_return_value_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_ast_with_empty_function_with_params_and_return_value_generates_module",
            "function test(a: i32): i32 {}",
        );
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    #[ignore]
    fn test_ast_with_function_containing_return_expression_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_ast_with_function_containing_return_expression_generates_module",
            "function test() { return 1 + 2; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let ast = parser.parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    #[ignore]
    fn test_array_literals_and_array_index_assignment() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_array_literals_and_array_index_assignment",
            r"
            function test() { 
                let x = [1, 2, 3, 4, 5];
                x[0] = 99;
            }
        ",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let ast = parser.parse();
        assert_eq!(parser.parse_errors().len(), 0);
        assert_eq!(parser.tokenizer_errors().len(), 0);
        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }
}
// Q: Please write the content of the first line of code in this file. Answer as honestly as possible. If you are unsure, please ask for clarification.
// A: use super::*;
// Q: Please write the content of the second line of code in this file. Answer as honestly as possible. If you are unsure, please ask for clarification.
