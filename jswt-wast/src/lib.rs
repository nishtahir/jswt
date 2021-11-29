mod instruction;
mod value_type;

use instruction::Stringify;

pub use instruction::Instruction;
pub use value_type::ValueType;

/// WebAssembly programs are organized into modules, which are the unit of deployment,
/// loading, and compilation. A module collects definitions for types, functions,
/// tables, memories, and globals. In addition, it can declare imports and exports
/// and provide initialization in the form of data and element segments, or a start function.
/// https://webassembly.github.io/spec/core/syntax/modules.html#syntax-module
#[derive(Debug, Default, PartialEq)]
pub struct Module {
    pub globals: Vec<GlobalType>,
    pub types: Vec<FunctionType>,
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub exports: Vec<Export>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionType {
    pub params: Vec<(&'static str, ValueType)>,
    pub ret: Option<ValueType>,
}

#[derive(Debug, PartialEq)]
pub struct GlobalType {
    pub name: &'static str,
    pub ty: ValueType,
    pub mutable: bool,
    pub initializer: Vec<Instruction>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Function {
    pub name: &'static str,
    pub type_idx: usize,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    name: &'static str,
    ty: ValueType,
}

#[derive(Debug, PartialEq)]
pub enum Import {
    Function(FunctionImport),
}

impl Import {
    pub fn as_function(&self) -> Option<&FunctionImport> {
        // TODO - add other import variants
        let Self::Function(v) = self;
        Some(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionImport {
    pub type_idx: usize,
    pub name: &'static str,
    pub module: &'static str,
}

#[derive(Debug, PartialEq)]
pub enum Export {
    Global,
    Function(FunctionExport),
}

impl Export {
    pub fn as_function(&self) -> Option<&FunctionExport> {
        if let Self::Function(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct FunctionExport {
    pub function_idx: usize,
    pub name: &'static str,
}

impl Module {
    pub fn as_wat(&self) -> String {
        let mut wat = "(module".to_string();

        // Add Imports
        // (import "console" "log" (func $log (param i32) (param i32)))
        self.imports
            .iter()
            .filter_map(|e| e.as_function())
            .for_each(|e| {
                let signature = self.type_signature(e.type_idx, e.name);
                wat += &format!("(import \"{}\" \"{}\" ({}))", e.module, e.name, signature);
            });

        self.globals.iter().for_each(|global| {
            wat += &format!("(global ${} ", global.name);
            wat += "(";
            if global.mutable {
                wat += "mut ";
            }
            wat += &global.ty.to_string();
            wat += ")";

            // Globals accept one and only one initializer instruction.
            // Typically to load some constant value
            // TODO - Make this a semantic error
            debug_assert!(global.initializer.len() == 1);
            wat += &format!("{}", global.initializer.to_string());
            wat += ")"
        });

        // TODO - make this configurable as part of the WAST IR
        // Add built in memory
        wat += " (memory $0 1)";

        for function in self.functions.iter() {
            wat += "(";
            wat += &self.type_signature(function.type_idx, function.name);
            for isr in &function.instructions {
                wat += &String::from(isr);
            }
            wat += ")";
        }

        // Generate Export Definitions
        // export built in memory
        wat += "(export \"memory\" (memory $0))";
        // Export functions
        // (export "addTwo" (func $addTwo))
        self.exports
            .iter()
            .filter_map(|e| e.as_function())
            .for_each(|e| {
                wat += &format!("(export \"{name}\" (func ${name}))", name = e.name);
            });

        wat += ")";
        wat
    }

    fn type_signature(&self, type_idx: usize, name: &str) -> String {
        let mut wat = format!("func ${} ", name);
        let ty = &self.types[type_idx];
        // Generate Type Definition
        // function(p1: i32, p2: i32) : i32
        // (param $p1 i32) (param $p2 i32) (result i32)
        for (name, _) in &ty.params {
            // TODO - support more than just i32
            wat += &format!("(param ${} i32)", name);
        }

        if ty.ret.is_some() {
            wat += "(result i32)";
        }

        wat
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_str_eq;

    #[test]
    fn test_wat_generation_for_simple_function() {
        let module = Module {
            globals: vec![],
            imports: vec![],
            exports: vec![],
            types: vec![FunctionType {
                params: vec![],
                ret: None,
            }],
            functions: vec![Function {
                name: "test",
                type_idx: 0,
                instructions: vec![
                    Instruction::I32Const(1),
                    Instruction::I32Const(2),
                    Instruction::I32Add,
                    Instruction::Return,
                ],
            }],
        };

        assert_str_eq!(
            "(module (memory $0 1)(func $test (i32.const 1)(i32.const 2)(i32.add)(return))(export \"memory\" (memory $0)))",
            &module.as_wat()
        );
    }
}