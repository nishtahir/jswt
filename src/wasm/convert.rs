use super::section::{FunctionType, ValType};
use crate::ast::{function::FunctionStmt, types::Type};

impl<'a> From<&FunctionStmt<'a>> for FunctionType<'a> {
    fn from(value: &FunctionStmt<'a>) -> Self {
        FunctionType {
            export: false,
            name: value.ident.value,
            params: value
                .params
                .iter()
                .map(|param| (&param.ty).into())
                .collect(),
            ret: (&value.returns).into(),
        }
    }
}

impl<'a> From<&Type<'_>> for ValType {
    fn from(_: &Type<'_>) -> Self {
        ValType::I32
    }
}
