use super::FunctionType;
use crate::wasm::Serialize;
use std::{error::Error, io::Write};

/// https://webassembly.github.io/spec/core/binary/modules.html#type-section
pub struct TypeSection<'a> {
    pub(crate) functions: Vec<FunctionType<'a>>,
}

impl<'a> TypeSection<'a> {
    pub fn new(functions: Vec<FunctionType<'a>>) -> Self {
        TypeSection { functions }
    }
}

impl<'a> Serialize for TypeSection<'a> {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        // Build type data
        let types: Vec<u8> = self
            .functions
            .iter()
            .flat_map(|f| f.serialize())
            .flatten()
            .collect();

        // The type section has the id 1.
        let mut buf = vec![
            0x01,
            (types.len() + 1) as u8,    // Size of the section
            self.functions.len() as u8, // Add number of declared types
        ];

        // Write section data
        buf.write(&types)?;

        Ok(buf)
    }
}
