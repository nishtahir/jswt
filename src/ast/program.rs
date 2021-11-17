use super::ident::Ident;

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    pub source_elements: SourceElements<'a>,
}

#[derive(Debug, PartialEq)]
pub struct SourceElements<'a> {
    pub source_elements: Vec<SourceElement<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum SourceElement<'a> {
    FunctionDeclaration(FunctionDeclarationElement<'a>),
    Statement(StatementElement<'a>),
}

impl<'a> From<StatementElement<'a>> for SourceElement<'a> {
    fn from(v: StatementElement<'a>) -> Self {
        Self::Statement(v)
    }
}

impl<'a> From<FunctionDeclarationElement<'a>> for SourceElement<'a> {
    fn from(v: FunctionDeclarationElement<'a>) -> Self {
        Self::FunctionDeclaration(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclarationElement<'a> {
    pub decorators: FunctionDecorators,
    pub ident: Ident<'a>,
    pub params: FormalParameterList<'a>,
    pub returns: Option<Ident<'a>>,
    pub body: FunctionBody<'a>,
}

impl<'a> FunctionDeclarationElement<'a> {
    pub fn new(
        decorators: FunctionDecorators,
        ident: Ident<'a>,
        params: FormalParameterList<'a>,
        returns: Option<Ident<'a>>,
        body: FunctionBody<'a>,
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
pub struct FormalParameterList<'a> {
    pub parameters: Vec<FormalParameterArg<'a>>,
}

impl<'a> FormalParameterList<'a> {
    pub fn new(parameters: Vec<FormalParameterArg<'a>>) -> Self {
        Self { parameters }
    }
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterArg<'a> {
    pub ident: Ident<'a>,
    pub type_annotation: Ident<'a>,
}

impl<'a> FormalParameterArg<'a> {
    pub fn new(ident: Ident<'a>, type_annotation: Ident<'a>) -> Self {
        Self {
            ident,
            type_annotation,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionBody<'a> {
    source_elements: SourceElements<'a>,
}

impl<'a> FunctionBody<'a> {
    pub fn new(source_elements: SourceElements<'a>) -> Self {
        Self { source_elements }
    }
}

#[derive(Debug, PartialEq)]
pub enum StatementElement<'a> {
    Block(BlockStatement<'a>),
    Empty(EmptyStatement),
    Variable(VariableStatement<'a>),
}

impl<'a> From<VariableStatement<'a>> for StatementElement<'a> {
    fn from(v: VariableStatement<'a>) -> Self {
        Self::Variable(v)
    }
}

impl<'a> From<EmptyStatement> for StatementElement<'a> {
    fn from(v: EmptyStatement) -> Self {
        Self::Empty(v)
    }
}

impl<'a> From<BlockStatement<'a>> for StatementElement<'a> {
    fn from(v: BlockStatement<'a>) -> Self {
        Self::Block(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement<'a> {
    pub statements: StatementList<'a>,
}

impl<'a> BlockStatement<'a> {
    pub fn new(statements: StatementList<'a>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct EmptyStatement {}

#[derive(Debug, PartialEq)]
pub struct VariableStatement<'a> {
    pub modifier: VariableModifier,
    pub target: AssignableElement<'a>,
}

impl<'a> VariableStatement<'a> {
    pub fn new(modifier: VariableModifier, target: AssignableElement<'a>) -> Self {
        Self { modifier, target }
    }
}

#[derive(Debug, PartialEq)]
pub struct StatementList<'a> {
    pub statements: Vec<StatementElement<'a>>,
}

impl<'a> StatementList<'a> {
    pub fn new(statements: Vec<StatementElement<'a>>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, PartialEq)]
pub enum VariableModifier {
    Let,
    Const,
}

#[derive(Debug, PartialEq)]
pub enum AssignableElement<'a> {
    Identifier(Ident<'a>),
}

impl<'a> From<Ident<'a>> for AssignableElement<'a> {
    fn from(v: Ident<'a>) -> Self {
        Self::Identifier(v)
    }
}
