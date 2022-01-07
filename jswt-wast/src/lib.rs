mod instruction;
mod value_type;

use std::borrow::Cow;

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
    pub params: Vec<(Cow<'static, str>, ValueType)>,
    pub ret: Option<ValueType>,
}

#[derive(Debug, PartialEq)]
pub struct GlobalType {
    pub name: Cow<'static, str>,
    pub ty: ValueType,
    pub mutable: bool,
    pub initializer: Instruction,
}

#[derive(Debug, Default, PartialEq)]
pub struct Function {
    pub name: Cow<'static, str>,
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
    pub name: Cow<'static, str>,
    pub module: Cow<'static, str>,
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
    pub name: Cow<'static, str>,
}

impl Module {
    pub fn as_wat(&self, minified: bool) -> String {
        let mut wat = "(module".to_string();

        // Add Imports
        // (import "console" "log" (func $log (param i32) (param i32)))
        self.imports
            .iter()
            .filter_map(|e| e.as_function())
            .for_each(|e| {
                let signature = self.type_signature(e.type_idx, e.name.as_ref());
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

            wat += &global.initializer.to_string();
            wat += ")"
        });

        // TODO - make this configurable as part of the WAST IR
        // Add built in memory
        wat += " (memory $0 1)";

        for function in self.functions.iter() {
            wat += "(";
            wat += &self.type_signature(function.type_idx, function.name.as_ref());
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

        if minified {
            return wat;
        }

        format_wat(wat)
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

// Barebones S expression formatter to make
// WAST out put easier to read.
fn format_wat(source: String) -> String {
    let mut result = String::from("");
    let mut indent = 0;
    let mut is_char_esc = false;
    let mut is_new_line = true;

    for ch in source.chars() {
        match ch {
            '(' => {
                if !is_new_line {
                    result += "\n";
                }
                if !is_char_esc {
                    result += &"  ".repeat(indent);
                    indent += 1;
                }
                is_new_line = false;
                is_char_esc = false;
                result.push(ch);
            }
            ')' => {
                is_char_esc = false;
                indent -= 1;
                result.push(ch);
            }
            '\\' => {
                is_char_esc = !is_char_esc;
                result.push(ch);
            }
            '\n' | '\r' => { /* skip */ }
            _ => {
                is_char_esc = false;
                result.push(ch)
            }
        }
    }

    result
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_snapshot;

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
                name: "test".into(),
                type_idx: 0,
                instructions: vec![Instruction::Return(Box::new(Instruction::I32Add(
                    Box::new(Instruction::I32Const(1)),
                    Box::new(Instruction::I32Const(2)),
                )))],
            }],
        };
        let actual = &module.as_wat(true);
        assert_snapshot!(actual);
    }

    #[test]
    fn test_format_wat() {
        let test ="(module (memory $0 1)(func $test (i32.const 1)(i32.const 2)(i32.add)(return))(export \"memory\" (memory $0)))";
        let actual = format_wat(test.to_owned());
        assert_snapshot!(actual);
    }
}
