use super::FunctionType;
use crate::wasm::Serialize;
use std::{error::Error, io::Write};

pub struct FunctionSection<'a> {
    pub(crate) functions: Vec<FunctionType<'a>>,
}

impl<'a> FunctionSection<'a> {
    pub fn new(functions: Vec<FunctionType<'a>>) -> Self {
        FunctionSection { functions }
    }
}

impl<'a> Serialize for FunctionSection<'a> {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let indexes: Vec<u8> = self
            .functions
            .iter()
            .enumerate()
            .map(|(i, _)| i as u8)
            .collect();

        let mut buf = vec![
            0x03,                       // Section Id
            (indexes.len() + 1) as u8,  // Size of the section
            self.functions.len() as u8, // Add number of declared functions
        ];

        // Encode indexes
        buf.write(&indexes)?;

        Ok(buf)
    }
}
