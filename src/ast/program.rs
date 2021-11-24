use super::ident::Ident;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub source_elements: SourceElements,
}

#[derive(Debug, PartialEq)]
pub struct SourceElements {
    pub source_elements: Vec<SourceElement>,
}

#[derive(Debug, PartialEq)]
pub enum SourceElement {
    FunctionDeclaration(FunctionDeclarationElement),
    Statement(StatementElement),
}

impl From<StatementElement> for SourceElement {
    fn from(v: StatementElement) -> Self {
        Self::Statement(v)
    }
}

impl From<FunctionDeclarationElement> for SourceElement {
    fn from(v: FunctionDeclarationElement) -> Self {
        Self::FunctionDeclaration(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclarationElement {
    pub decorators: FunctionDecorators,
    pub ident: Ident,
    pub params: FormalParameterList,
    pub returns: Option<Ident>,
    pub body: FunctionBody,
}

impl FunctionDeclarationElement {
    pub fn new(
        decorators: FunctionDecorators,
        ident: Ident,
        params: FormalParameterList,
        returns: Option<Ident>,
        body: FunctionBody,
    ) -> Self {
        Self {
            decorators,
            ident,
            params,
            returns,
            body,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecorators {
    pub export: bool,
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterList {
    pub parameters: Vec<FormalParameterArg>,
}

impl FormalParameterList {
    pub fn new(parameters: Vec<FormalParameterArg>) -> Self {
        Self { parameters }
    }
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterArg {
    pub ident: Ident,
    pub type_annotation: Ident,
}

impl FormalParameterArg {
    pub fn new(ident: Ident, type_annotation: Ident) -> Self {
        Self {
            ident,
            type_annotation,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionBody {
    pub source_elements: SourceElements,
}

impl FunctionBody {
    pub fn new(source_elements: SourceElements) -> Self {
        Self { source_elements }
    }
}

#[derive(Debug, PartialEq)]
pub enum StatementElement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    Return(ReturnStatement),
    Variable(VariableStatement),
}

impl From<ReturnStatement> for StatementElement {
    fn from(v: ReturnStatement) -> Self {
        Self::Return(v)
    }
}

impl From<VariableStatement> for StatementElement {
    fn from(v: VariableStatement) -> Self {
        Self::Variable(v)
    }
}

impl From<EmptyStatement> for StatementElement {
    fn from(v: EmptyStatement) -> Self {
        Self::Empty(v)
    }
}

impl From<BlockStatement> for StatementElement {
    fn from(v: BlockStatement) -> Self {
        Self::Block(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: StatementList,
}

impl BlockStatement {
    pub fn new(statements: StatementList) -> Self {
        Self { statements }
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct EmptyStatement {}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub expression: SingleExpression,
}

impl ReturnStatement {
    pub fn new(expression: SingleExpression) -> Self {
        Self { expression }
    }
}

#[derive(Debug, PartialEq)]
pub struct VariableStatement {
    pub modifier: VariableModifier,
    pub target: AssignableElement,
    pub expression: SingleExpression,
}

impl VariableStatement {
    pub fn new(
        modifier: VariableModifier,
        target: AssignableElement,
        expression: SingleExpression,
    ) -> Self {
        Self {
            modifier,
            target,
            expression,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StatementList {
    pub statements: Vec<StatementElement>,
}

impl StatementList {
    pub fn new(statements: Vec<StatementElement>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, PartialEq)]
pub enum VariableModifier {
    Let,
    Const,
}

#[derive(Debug, PartialEq)]
pub enum AssignableElement {
    Identifier(Ident),
}

impl From<Ident> for AssignableElement {
    fn from(v: Ident) -> Self {
        Self::Identifier(v)
    }
}

#[derive(Debug, PartialEq)]
pub enum SingleExpression {
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(StringLiteral),
    Number(NumberLiteral),
    Boolean(BooleanLiteral),
}

impl From<StringLiteral> for Literal {
    fn from(v: StringLiteral) -> Self {
        Self::String(v)
    }
}

impl From<NumberLiteral> for Literal {
    fn from(v: NumberLiteral) -> Self {
        Self::Number(v)
    }
}

impl From<BooleanLiteral> for Literal {
    fn from(v: BooleanLiteral) -> Self {
        Self::Boolean(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, PartialEq)]

pub struct NumberLiteral {
    pub value: i32,
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral {
    pub value: &'static str,
}
