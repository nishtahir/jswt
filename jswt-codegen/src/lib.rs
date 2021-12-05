use std::borrow::Borrow;

use jswt_ast::*;
use jswt_common::SymbolTable;
use jswt_wast::*;

#[derive(Debug)]
pub struct CodeGenerator {
    module: Module,
    /// The architecture here assumes that this is an instruction scope stack
    /// any lexical scope that wishes to recieve an emitted set of instructions
    /// should push a new scope to the stack and pop the scope before pushing their own
    /// instruction to the stack
    scopes: Vec<InstructionScope>,
    symbols: SymbolTable<&'static str, WastSymbol>,
    label_counter: usize,
}

#[derive(Debug)]
struct InstructionScope {
    target: Option<InstructionScopeTarget>,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
enum InstructionScopeTarget {
    // Control flow Ifs
    Then,
    Else,
    Loop,
    Function(&'static str),
    Local(&'static str),
    Global(&'static str),
}

#[derive(Debug, PartialEq)]
pub enum WastSymbol {
    Function(&'static str),
    Param(&'static str, ValueType),
    Local(&'static str, ValueType),
    Global(&'static str, ValueType),
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self {
            module: Default::default(),
            scopes: Default::default(),
            symbols: SymbolTable::new(vec![]),
            label_counter: 0,
        }
    }
}

impl CodeGenerator {
    #[cfg(test)]
    pub fn generate_module(&mut self, ast: &Ast) -> &Module {
        // TODO - we should be accepting builtins externally from the env
        // This is a stop gap so tests don't break
        self.visit_program(&ast.program);
        &self.module
    }

    #[cfg(not(test))]
    pub fn generate_module(&mut self, ast: &Ast) -> &Module {
        // Push builtins
        let println_type_idx = self.push_type(FunctionType {
            params: vec![("value", ValueType::I32)],
            ret: None,
        });
        self.push_import(Import::Function(FunctionImport {
            name: "println",
            type_idx: println_type_idx,
            module: "env",
        }));

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
    fn push_instruction_scope(&mut self, target: Option<InstructionScopeTarget>) {
        self.scopes.push(InstructionScope {
            target,
            instructions: vec![],
        });
    }

    fn set_instruction_scope_target(&mut self, target: Option<InstructionScopeTarget>) {
        let scope = self.scopes.last_mut().unwrap();
        scope.target = target;
    }

    /// Pops an Instruction scope from the stack
    fn pop_instruction_scope(&mut self) -> Option<InstructionScope> {
        self.scopes.pop()
    }
}

impl StatementVisitor for CodeGenerator {
    fn visit_program(&mut self, node: &Program) {
        // Push global scope
        self.symbols.push_scope();
        self.visit_source_elements(&node.source_elements);
        // Pop global scope from stack
        debug_assert!(self.symbols.depth() == 1);
        self.symbols.pop_scope();
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
        self.visit_single_expression(&node.condition);

        // Handle the consequence instructions
        self.push_instruction_scope(Some(InstructionScopeTarget::Then));
        self.visit_statement_element(&node.consequence);
        let cons = self.pop_instruction_scope().unwrap();

        // Handle the alternative instructions
        self.push_instruction_scope(Some(InstructionScopeTarget::Else));
        if let Some(alternative) = node.alternative.borrow() {
            self.visit_statement_element(alternative);
        }
        let alt = self.pop_instruction_scope().unwrap();
        self.push_instruction(Instruction::If(cons.instructions, alt.instructions));
    }

    fn visit_iteration_statement(&mut self, node: &IterationStatement) {
        match node {
            IterationStatement::While(elem) => self.visit_while_iteration_element(elem),
        }
    }

    fn visit_while_iteration_element(&mut self, node: &WhileIterationElement) {
        let loop_label = self.label_counter;
        self.label_counter += 1;

        self.push_instruction_scope(Some(InstructionScopeTarget::Loop));

        // First push the expression result onto the stack
        self.visit_single_expression(&node.expression);

        // Push an if scope for branching
        self.push_instruction_scope(Some(InstructionScopeTarget::Then));
        // Add the statements to the scope
        self.visit_statement_element(&node.statement);

        // Add the branch back to the top of the loop to test
        self.push_instruction(Instruction::Br(loop_label));

        let if_scope = self.pop_instruction_scope().unwrap();
        self.push_instruction(Instruction::If(if_scope.instructions, vec![]));

        let loop_scope = self.pop_instruction_scope().unwrap();
        self.push_instruction(Instruction::Loop(loop_label, loop_scope.instructions));
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        self.visit_single_expression(&node.expression);
        self.push_instruction(Instruction::Return);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        // Push scope for the initializer
        self.push_instruction_scope(None);
        self.visit_assignable_element(&node.target);
        self.visit_single_expression(&node.expression);
        let initializer_sope = self.pop_instruction_scope().unwrap();
        match initializer_sope.target {
            Some(InstructionScopeTarget::Local(name)) => {
                self.push_instruction(Instruction::LocalSet(name, initializer_sope.instructions));
            }
            Some(InstructionScopeTarget::Global(name)) => {
                // For global variable declarations we want to emit a global rather
                // than an instruction.
                self.push_global(GlobalType {
                    name,
                    ty: ValueType::I32,
                    mutable: true, // TODO - check mutability
                    initializer: initializer_sope.instructions,
                });
            }
            _ => todo!(),
        }
    }

    fn visit_expression_statement(&mut self, node: &ExpressionStatement) {
        self.visit_single_expression(&node.expression)
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
        for (_, arg) in node.params.parameters.iter().enumerate() {
            let name = arg.ident.value;
            // Add to symbol table
            self.symbols
                .define(arg.ident.value, WastSymbol::Param(name, ValueType::I32));
            // Todo, convert to ValueType
            type_params.push((arg.ident.value, ValueType::I32))
        }

        // Resolve return Value
        let ty = FunctionType {
            params: type_params,
            ret: node.returns.as_ref().map(|_| ValueType::I32),
        };
        // Add type definition to type index
        let type_idx = self.push_type(ty);

        // Push a new Instruction scope to hold emitted instructions
        self.push_instruction_scope(None);

        // TODO - this was only a prototye abstract this out
        // Resolve the content of the annotation
        if let Some(annotation) = &node.decorators.annotation {
            match annotation.name.value {
                // The "wast" annotation allows the developer to emit
                // WAST instructions directily into the instruction scope
                // of the function.
                "wast" => match &annotation.expr {
                    SingleExpression::Literal(Literal::String(string_lit)) => {
                        self.push_instruction(Instruction::RawWast(string_lit.value));
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }

        // Generate instructions for the current scope context
        self.visit_function_body(&node.body);

        // Function generation is done. Pop the current instructions scope
        // and commit it to the module
        let mut scope = self.pop_instruction_scope().unwrap();
        // Push our locals to the instructions
        self.symbols
            .symbols_in_current_scope()
            .iter()
            .for_each(|sym| match sym {
                WastSymbol::Function(_) => {}
                WastSymbol::Param(_, _) => {}
                WastSymbol::Global(_, _) => {}
                WastSymbol::Local(name, ty) => {
                    scope.instructions.insert(0, Instruction::Local(name, *ty))
                }
            });

        let function_name = node.ident.value;
        let function = Function {
            name: function_name,
            type_idx,
            instructions: scope.instructions,
        };
        let function_idx = self.push_function(function);

        // Generate export descriptor if the function is marked for export
        if node.decorators.export {
            let desc = FunctionExport {
                function_idx,
                name: function_name,
            };
            self.push_export(Export::Function(desc));
        }

        // Pop the current function scope from the symbol table
        self.symbols.pop_scope();
    }

    fn visit_function_body(&mut self, node: &FunctionBody) {
        self.visit_source_elements(&node.source_elements);
    }
}
impl ExpressionVisitor<()> for CodeGenerator {
    fn visit_assignment_expression(&mut self, node: &BinaryExpression) {
        // Push scope for value
        self.push_instruction_scope(None);
        self.visit_single_expression(node.right.borrow());
        let rhs_scope = self.pop_instruction_scope().unwrap();

        match node.left.borrow() {
            SingleExpression::Identifier(exp) => {
                let name = exp.ident.value;
                if self.symbols.lookup_global(&name).is_some() {
                    self.push_instruction(Instruction::GlobalSet(name, rhs_scope.instructions))
                } else {
                    self.push_instruction(Instruction::LocalSet(name, rhs_scope.instructions))
                }
                // figure out the scope of the variable
            }
            _ => todo!(),
        };
    }

    fn visit_assignable_element(&mut self, elem: &AssignableElement) {
        // Figure out the target for an assignment
        // We assume that this is the target for
        // the current instruction scope
        match elem {
            AssignableElement::Identifier(ident) => {
                let name = ident.value;

                // Check if this element has been defined
                if self.symbols.lookup(&name).is_none() {
                    if self.symbols.depth() == 1 {
                        self.symbols
                            .define(name, WastSymbol::Global(name, ValueType::I32));
                    } else {
                        self.symbols
                            .define(name, WastSymbol::Local(name, ValueType::I32));
                    }
                }

                // Guaranteed exists
                // TODO fix immutable and mutable borrow
                let sym = self.symbols.lookup(&name).unwrap();

                match sym {
                    WastSymbol::Function(_) => todo!(),
                    WastSymbol::Param(name, _) | WastSymbol::Local(name, _) => {
                        self.set_instruction_scope_target(Some(InstructionScopeTarget::Local(name)))
                    }
                    WastSymbol::Global(name, _) => self
                        .set_instruction_scope_target(Some(InstructionScopeTarget::Global(name))),
                }
            }
        }
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) {
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
        }
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        self.visit_single_expression(&node.left);
        self.visit_single_expression(&node.right);

        // TODO - type check before pushing op
        let isr = match node.op {
            BinaryOperator::Plus(_) => Instruction::I32Add,
            BinaryOperator::Minus(_) => Instruction::I32Sub,
            BinaryOperator::Mult(_) => Instruction::I32Mul,
            BinaryOperator::Equal(_) => Instruction::I32Eq,
            BinaryOperator::NotEqual(_) => Instruction::I32Neq,
            BinaryOperator::Div(_) => Instruction::I32Div,
            BinaryOperator::And(_) => Instruction::I32And,
            BinaryOperator::Or(_) => Instruction::I32Or,
            BinaryOperator::Greater(_) => Instruction::I32Gt,
            BinaryOperator::GreaterEqual(_) => Instruction::I32Ge,
            BinaryOperator::Less(_) => Instruction::I32Lt,
            BinaryOperator::LessEqual(_) => Instruction::I32Le,
            BinaryOperator::Assign(_) => todo!(),
        };
        self.push_instruction(isr)
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        let target = node.ident.value;
        if self.symbols.lookup_current(&target).is_some() {
            self.push_instruction(Instruction::LocalGet(target));
        } else {
            self.push_instruction(Instruction::GlobalGet(target));
        }
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) {
        if let SingleExpression::Identifier(ident_exp) = node.ident.borrow() {
            // Push a new instruction scope for the
            // function call
            let function_name = ident_exp.ident.value;
            self.push_instruction_scope(Some(InstructionScopeTarget::Function(function_name)));
            for exp in node.arguments.arguments.iter() {
                self.visit_single_expression(exp);
            }

            // Pop the scope containing the args instructions
            let scope = self.pop_instruction_scope().unwrap();
            self.push_instruction(Instruction::Call(function_name, scope.instructions));
        }
    }

    fn visit_literal(&mut self, node: &Literal) {
        match node {
            Literal::String(_) => todo!(),
            Literal::Number(lit) => self.push_instruction(Instruction::I32Const(lit.value)),
            Literal::Boolean(lit) => match lit.value {
                // Boolean values in WebAssembly are represented as values of type i32. In a boolean context,
                // such as a br_if condition, any non-zero value is interpreted as true and 0 is interpreted as false.
                true => self.push_instruction(Instruction::I32Const(1)),
                false => self.push_instruction(Instruction::I32Const(0)),
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::*;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_empty_ast_generates_empty_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_ast_with_empty_function_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() {}");
        let ast = Parser::new(&mut tokenizer).parse();
        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_ast_with_empty_function_with_params_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(a: i32) {}");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }
    #[test]
    fn test_ast_with_empty_function_with_params_and_return_value_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(a: i32): i32 {}");
        let ast = Parser::new(&mut tokenizer).parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_ast_with_function_containing_return_expression_generates_module() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { return 1 + 2; }");
        let mut parser = Parser::new(&mut tokenizer);
        let ast = parser.parse();

        let mut generator = CodeGenerator::default();
        let actual = generator.generate_module(&ast);
        assert_debug_snapshot!(actual);
    }
}
