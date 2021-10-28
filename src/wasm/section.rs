use std::{error::Error, io::Write};

use super::Serialize;

pub enum Section {
    Type(TypeSection),
    Function(FunctionSection),
    Export(ExportSection),
}

impl Section {
    pub fn type_section(functions: Vec<FunctionType>) -> Self {
        Section::Type(TypeSection::new(functions))
    }

    pub fn function_section() -> Self {
        Section::Function(FunctionSection::new())
    }

    pub fn export_section() -> Self {
        Section::Export(ExportSection::new())
    }
}

impl Serialize for Section {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        match self {
            Section::Type(inner) => inner.serialize(),
            Section::Function(inner) => inner.serialize(),
            Section::Export(inner) => inner.serialize(),
        }
    }
}

/// https://webassembly.github.io/spec/core/binary/modules.html#type-section
pub struct TypeSection {
    functions: Vec<FunctionType>,
}

impl TypeSection {
    pub fn new(functions: Vec<FunctionType>) -> Self {
        TypeSection { functions }
    }
}

impl Serialize for TypeSection {
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

pub struct FunctionSection {
    functions: Vec<FunctionType>,
}

impl FunctionSection {
    pub fn new() -> Self {
        FunctionSection {
            functions: vec![FunctionType {
                export: false,
                params: vec![ValType::I32, ValType::I32],
                ret: ValType::I32,
            }],
        }
    }
}

impl Serialize for FunctionSection {
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

pub struct ExportSection {}

impl ExportSection {
    pub fn new() -> Self {
        ExportSection {}
    }
}

impl Serialize for ExportSection {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        todo!()
    }
}

pub enum ValType {
    I32,
    F32,
    Void,
}

impl From<&ValType> for u8 {
    fn from(ty: &ValType) -> Self {
        match ty {
            ValType::I32 => 0x7fu8,
            ValType::F32 => 0x7du8,
            ValType::Void => 0x00u8,
        }
    }
}

impl From<ValType> for u8 {
    fn from(ty: ValType) -> Self {
        match ty {
            ValType::I32 => 0x7fu8,
            ValType::F32 => 0x7du8,
            ValType::Void => 0x00u8,
        }
    }
}

pub struct FunctionType {
    pub export: bool,
    pub params: Vec<ValType>,
    pub ret: ValType,
}

impl Serialize for FunctionType {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let mut res = vec![
            0x60u8, // Function type ID
        ];

        let params = self
            .params
            .iter()
            .map(|param| param.into())
            .collect::<Vec<u8>>();

        // Param count
        res.push(self.params.len() as u8);
        // number of return values
        res.write(&params)?;

        // Return value count
        if let ValType::Void = self.ret {
            // Void functions do not count towards
            // return value count
            res.push(0x00);
        } else {
            // only single return values are supported for now
            res.push(0x01);
            res.push((&self.ret).into());
        };

        Ok(res)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_serialize_type_section() {
        let section = TypeSection {
            functions: vec![FunctionType {
                export: false,
                params: vec![ValType::I32, ValType::I32],
                ret: ValType::I32,
            }],
        };

        let actual = section.serialize().unwrap();
        assert_eq!(
            actual,
            [0x01, 0x07, 0x01, 0x60, 0x02, 0x7F, 0x7F, 0x01, 0x7F]
        );
    }

    #[test]
    fn test_serialize_type_section_with_2_functions() {
        let section = TypeSection {
            functions: vec![
                FunctionType {
                    export: false,
                    params: vec![ValType::I32, ValType::I32],
                    ret: ValType::I32,
                },
                FunctionType {
                    export: false,
                    params: vec![],
                    ret: ValType::Void,
                },
            ],
        };

        let actual = section.serialize().unwrap();
        assert_eq!(
            actual,
            [0x01, 0x0a, 0x02, 0x60, 0x02, 0x7F, 0x7F, 0x01, 0x7F, 0x60, 0x00, 0x00]
        );
    }

    #[test]
    fn test_serialize_function_type_with_2_i32_params() {
        let function = FunctionType {
            export: false,
            params: vec![ValType::I32, ValType::I32],
            ret: ValType::I32,
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f]);
    }

    #[test]
    fn test_serialize_void_function_type_with_2_i32_params() {
        let function = FunctionType {
            export: false,
            params: vec![ValType::I32, ValType::I32],
            ret: ValType::Void,
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x60, 0x02, 0x7f, 0x7f, 0x00]);
    }

    #[test]
    fn test_serialize_void_function_type_with_0_params() {
        let function = FunctionType {
            export: false,
            params: vec![],
            ret: ValType::Void,
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x60, 0x00, 0x00]);
    }

    #[test]
    fn test_serialize_function_section_with_no_functions() {
        let function = FunctionSection { functions: vec![] };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x03, 0x01, 0x00]);
    }

    #[test]
    fn test_serialize_function_section_with_1_void_function() {
        let function = FunctionSection {
            functions: vec![FunctionType {
                export: false,
                params: vec![],
                ret: ValType::Void,
            }],
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x03, 0x02, 0x01, 0x00]);
    }

    #[test]
    fn test_serialize_function_section_with_2_functions() {
        let function = FunctionSection {
            functions: vec![
                FunctionType {
                    export: false,
                    params: vec![ValType::I32, ValType::I32],
                    ret: ValType::I32,
                },
                FunctionType {
                    export: false,
                    params: vec![],
                    ret: ValType::Void,
                },
            ],
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x03, 0x03, 0x2, 0x00, 0x01]);
    }
}
