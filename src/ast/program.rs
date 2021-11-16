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
    Statement(StatementElement),
}

impl<'a> From<StatementElement> for SourceElement<'a> {
    fn from(v: StatementElement) -> Self {
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
pub enum StatementElement {
    Block(BlockStatement),
    Empty(EmptyStatement),
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

#[derive(Debug, PartialEq)]
pub struct EmptyStatement {}

impl EmptyStatement {
    pub fn new() -> Self {
        Self {}
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
