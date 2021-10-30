use super::FunctionType;
use crate::wasm::Serialize;
use std::{error::Error, io::Write};

pub struct ExportSection<'a> {
    pub(crate) functions: Vec<FunctionType<'a>>,
}

impl<'a> ExportSection<'a> {
    pub fn new(functions: Vec<FunctionType<'a>>) -> Self {
        ExportSection { functions }
    }

    fn export_description((index, function): (usize, &FunctionType)) -> Vec<u8> {
        let mut desc = vec![];

        desc.push(function.name.len() as u8);
        desc.write_all(function.name.as_bytes()).unwrap();

        // Function export identifier
        desc.push(0x00);
        // index function index
        desc.push(index as u8);
        desc
    }
}

impl<'a> Serialize for ExportSection<'a> {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        // for now export everything
        let exports = self.functions.len();

        let indexes: Vec<u8> = self
            .functions
            .iter()
            .enumerate()
            .map(Self::export_description)
            .flatten()
            .collect();

        let mut buf = vec![
            0x07,                      // Section Id
            (indexes.len() + 1) as u8, // Size of the section
            exports as u8,             // Add number of exports functions
        ];

        // Encode indexes
        buf.write(&indexes)?;

        Ok(buf)
    }
}
