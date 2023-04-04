use std::collections::BTreeMap;

use jswt_common::{Span, Type};

#[derive(Debug, Default)]
pub struct TypeMap {
    types: BTreeMap<Span, Type>,
}

impl TypeMap {
    pub fn insert_owned(&mut self, span: Span, ty: Type) {
        self.types.insert(span, ty);
    }

    pub fn insert(&mut self, span: &Span, ty: &Type) {
        self.insert_owned(span.clone(), ty.clone());
    }

    pub fn get(&self, span: &Span) -> &Type {
        self.types.get(span).unwrap_or(&Type::Unknown)
    }
}
