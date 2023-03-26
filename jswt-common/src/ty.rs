use std::borrow::Cow;

trait SizedType {
    fn size(&self) -> usize;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    Binding(Cow<'static, str>),
    Unknown,
}

impl Type {
    // Primitive type bindings
    pub const PTR: Type = Type::Binding(Cow::Borrowed("ptr"));
    pub const I32: Type = Type::Binding(Cow::Borrowed("i32"));
    pub const F32: Type = Type::Binding(Cow::Borrowed("f32"));
    pub const BOOLEAN: Type = Type::Binding(Cow::Borrowed("boolean"));
    pub const VOID: Type = Type::Binding(Cow::Borrowed("void"));

    // Reference type bindings
    pub const STRING: Type = Type::Binding(Cow::Borrowed("String"));
    pub const ARRAY: Type = Type::Binding(Cow::Borrowed("Array"));
}

pub trait Typeable {
    fn ty(&self) -> Type;
    fn binding(&self) -> Option<Cow<'static, str>>;
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Binding(inner) => inner.to_string(),
            Type::Unknown => "Unknown".to_string(),
        }
    }
}
