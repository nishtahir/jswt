use super::section::{FunctionType, ValType};
use crate::ast::{function::FunctionStmt, types::Type};

impl<'a> From<&FunctionStmt<'a>> for FunctionType {
    fn from(value: &FunctionStmt) -> Self {
        FunctionType {
            export: false,
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
