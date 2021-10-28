use crate::ast::statement::Statement;
use crate::ast::Ast;
use crate::wasm::section::FunctionType;

use super::section::Section;
use super::Serialize;
use std::error::Error;
use std::io::Write;
use std::vec;

/// https://webassembly.github.io/spec/core/binary/modules.html#binary-module

pub const MODULE_HEADER: &[u8] = &[0x00, 0x61, 0x73, 0x6d];
pub const MODULE_VERSION: &[u8] = &[0x01, 0x00, 0x00, 0x00];

pub struct Module {
    pub segments: Vec<Section>,
}

impl Module {
    pub fn new(ast: Ast) -> Self {
        let types: Vec<FunctionType> = ast
            .statements
            .iter()
            .filter_map(|statement| statement.as_function())
            .map(|function| function.into())
            .collect();
        Self {
            segments: vec![
                Section::type_section(types),
                // Section::function_section()
            ],
        }
    }
}

impl Serialize for Module {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let mut data = vec![];

        // The encoding of a module starts with a preamble containing a 4-byte magic number (the string ‘∖𝟶𝚊𝚜𝚖’) and a version field.
        // The current version of the WebAssembly binary format is 1.
        data.write_all(MODULE_HEADER)?;
        data.write_all(MODULE_VERSION)?;

        let sections = self
            .segments
            .iter()
            .flat_map(|s| s.serialize())
            .flatten()
            .collect::<Vec<u8>>();

        data.write(&sections)?;

        Ok(data)
    }
}
