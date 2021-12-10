use std::fmt::Display;

use crate::ValueType;

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Local(&'static str, ValueType),
    LocalGet(&'static str),
    LocalSet(&'static str, Box<Instruction>),
    GlobalSet(&'static str, Box<Instruction>),
    GlobalGet(&'static str),
    I32Const(i32),
    I32Add(Box<Instruction>, Box<Instruction>),
    I32Sub(Box<Instruction>, Box<Instruction>),
    I32Mul(Box<Instruction>, Box<Instruction>),
    I32Div(Box<Instruction>, Box<Instruction>),
    I32Eq(Box<Instruction>, Box<Instruction>),
    I32Neq(Box<Instruction>, Box<Instruction>),
    I32And(Box<Instruction>, Box<Instruction>),
    I32Or(Box<Instruction>, Box<Instruction>),
    I32Xor(Box<Instruction>, Box<Instruction>),
    I32Gt(Box<Instruction>, Box<Instruction>),
    I32Ge(Box<Instruction>, Box<Instruction>),
    I32Lt(Box<Instruction>, Box<Instruction>),
    I32Le(Box<Instruction>, Box<Instruction>),
    Block(usize, Vec<Instruction>),
    Return(Box<Instruction>),
    If(Box<Instruction>, Vec<Instruction>, Vec<Instruction>),
    Call(&'static str, Vec<Instruction>),
    Loop(usize, Vec<Instruction>),
    BrLoop(usize),
    BrBlock(usize),
    Noop,
    // A meta instruction not part of the wasm specification but
    // an instruction to the code generator to inline raw instructions
    // exactly as they are provided.
    // Warning: No checks are performed on the input before they are inlined
    RawWast(&'static str),
    // Synthetic return intended to exit the function with the
    // '$return' value set
    SynthReturn,
}

impl Instruction {
    /// Returns `true` if the instruction is [`Return`].
    ///
    /// [`Return`]: Instruction::Return
    pub fn is_return(&self) -> bool {
        matches!(self, Self::Return(..))
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from(self))
    }
}

impl From<&Instruction> for String {
    fn from(isr: &Instruction) -> Self {
        match isr {
            Instruction::I32Const(value) => format!("(i32.const {})", value),
            Instruction::I32Add(lhs, rhs) => format!("(i32.add {} {})", *lhs, *rhs),
            Instruction::I32Sub(lhs, rhs) => format!("(i32.sub {} {})", *lhs, *rhs),
            Instruction::I32Mul(lhs, rhs) => format!("(i32.mul {} {})", *lhs, *rhs),
            Instruction::I32Div(lhs, rhs) => format!("(i32.div_s {} {})", *lhs, *rhs),
            Instruction::I32Eq(lhs, rhs) => format!("(i32.eq {} {})", *lhs, *rhs),
            Instruction::I32Neq(lhs, rhs) => format!("(i32.ne {} {})", *lhs, *rhs),
            Instruction::I32Gt(lhs, rhs) => format!("(i32.gt_s {} {})", *lhs, *rhs),
            Instruction::I32Ge(lhs, rhs) => format!("(i32.ge_s {} {})", *lhs, *rhs),
            Instruction::I32Lt(lhs, rhs) => format!("(i32.lt_s {} {})", *lhs, *rhs),
            Instruction::I32Le(lhs, rhs) => format!("(i32.le_s {} {})", *lhs, *rhs),
            Instruction::I32And(lhs, rhs) => format!("(i32.and {} {})", *lhs, *rhs),
            Instruction::I32Or(lhs, rhs) => format!("(i32.or {} {})", *lhs, *rhs),
            Instruction::I32Xor(lhs, rhs) => format!("(i32.xor {} {})", *lhs, *rhs),
            Instruction::Return(instruction) => {
                // Set the synthetic value and break into the function block scope
                format!("(local.set $return {}) (br $blk0)", *instruction)
            }
            Instruction::Call(name, args) => {
                format!("(call ${} {})", name, args.to_string())
            }
            Instruction::RawWast(text) => text.to_string(),
            Instruction::LocalGet(name) => format!("(local.get ${})", name),
            Instruction::LocalSet(name, args) => {
                format!("(local.set ${} {})", name, args.to_string())
            }
            Instruction::GlobalSet(name, args) => {
                format!("(global.set ${} {})", name, args.to_string())
            }
            Instruction::Local(name, ty) => format!("(local ${} {})", name, ty),
            Instruction::If(cond, cons, alt) => {
                let mut stmt = "(if ".to_string();
                // https://github.com/WebAssembly/wabt/issues/1075
                // The wat format requires that you annotate any blocks that return values with their signature.
                // If no signature is provided, it is assumed that the block has no parameters and no results.
                // let cons_returns = cons.iter().any(|i| i.is_return());
                // let alt_returns = alt.iter().any(|i| i.is_return());
                // if cons_returns && alt_returns {
                //     stmt += "(result i32)";
                // }

                stmt += &format!("{}", *cond);
                stmt += &format!("(then {}) (else {})", cons.to_string(), alt.to_string());
                stmt += ")";
                stmt
            }
            Instruction::GlobalGet(name) => format!("(global.get ${})", name),
            Instruction::Loop(label, args) => format!("(loop $loop{} {})", label, args.to_string()),
            Instruction::BrBlock(label) => format!("(br $blk{})", label),
            Instruction::BrLoop(label) => format!("(br $loop{})", label),
            Instruction::Noop => "".into(),
            Instruction::SynthReturn => "(return (local.get $return))".into(),
            Instruction::Block(label, args) => {
                format!("(block $blk{} {})", label, args.to_string())
            }
        }
    }
}

impl Stringify for Vec<Instruction> {
    fn to_string(&self) -> String {
        self.iter()
            .map(String::from)
            .collect::<Vec<String>>()
            .join(" ")
    }
}

pub trait Stringify {
    fn to_string(&self) -> String;
}
