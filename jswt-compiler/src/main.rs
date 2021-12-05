#[macro_use]
extern crate clap;

use clap::Arg;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::exit;
use std::rc::Rc;
use wasmer::{imports, Function, Instance, MemoryView, Module as WasmerModule, Store};

use jswt_codegen::CodeGenerator;
use jswt_errors::{print_parser_error, print_semantic_error, print_tokenizer_error};
use jswt_parser::Parser;
use jswt_semantics::Resolver;
use jswt_tokenizer::Tokenizer;

fn main() {
    let matches = app_from_crate!()
        .arg(
            Arg::with_name("output")
                .short("o")
                .help("Write output to file")
                .required(false)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("file")
                .help("Input file to begin compiling")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("log-mem")
                .long("log-mem")
                .required(false)
                .takes_value(false)
                .help("Log the memory layout after execution"),
        )
        .get_matches();

    let input = match matches.value_of("file") {
        Some(it) => PathBuf::from(it)
            .canonicalize()
            .expect("Failed to determine canonical input"),
        _ => return,
    };

    let output = match matches.value_of("output") {
        Some(it) => PathBuf::from(it),
        None => input.clone(),
    };

    // Main cache of source files that have been read for compilation
    // It may be worth building an abstraction around this to help with resolving paths
    // for imports and preventing issues like duplicate file reads
    let source_map = Rc::new(RefCell::new(HashMap::new()));

    // Let binding to prevent the ref being dropped before getting passed to the tokenizer
    let mut tokenizer = Tokenizer::new(source_map.clone());
    tokenizer.push_source(&input);

    // parse tokens and generate AST
    let mut parser = Parser::new(&mut tokenizer);
    let ast = parser.parse();

    // Write AST for debugging
    fs::write(output.with_extension("ast"), format!("{:#?}", ast)).unwrap();

    let mut has_errors = false;
    let parser_errors = parser.errors();

    // Report errors
    for error in parser_errors.1 {
        has_errors = true;
        print_tokenizer_error(&error, &source_map.clone().borrow());
    }

    for error in parser_errors.0 {
        has_errors = true;
        print_parser_error(&error, &source_map.clone().borrow());
    }

    // Semantic analytis pass
    let mut resolver = Resolver::default();
    resolver.resolve(&ast);

    for error in resolver.errors() {
        has_errors = true;
        print_semantic_error(&error, &source_map.borrow())
    }

    if has_errors {
        exit(1);
    }

    let mut code_gen = CodeGenerator::default();
    let module = code_gen.generate_module(&ast);

    // Write generated wasm AST for debugging
    let wast = module.as_wat();
    fs::write(output.with_extension("wast"), &wast).unwrap();

    // Embed wasmer runtime and execute generated wasm
    let store = Store::default();
    let module = match WasmerModule::new(&store, wast) {
        Ok(module) => module,
        Err(e) => {
            panic!("{}", e);
        }
    };

    let import_object = imports! {
        "env" => {
            "println" => Function::new_native(&store, env_println)
        },
    };
    let instance = Instance::new(&module, &import_object).unwrap();

    // Assume main is a function that accepts no args and returns an i32
    // The returned i32 is the exit code
    // function main(): i32 { return 0; } // OK
    let main = instance.exports.get_function("main").unwrap();
    let result = match main.call(&[]) {
        Ok(result) => result,
        Err(e) => {
            panic!("{}", e);
        }
    };

    if matches.is_present("log-mem") {
        // Log memory contents to a file
        let memory = instance.exports.get_memory("memory").unwrap();
        let view: MemoryView<u8> = memory.view();
        let mem = view
            // log the memory with 16 bytes per row
            // This will give us 4096 rows per page
            .chunks(16)
            .map(|chunk| {
                chunk
                    .iter()
                    .map(Cell::get)
                    .map(|val| format!("{:02x?}", val))
                    .collect::<Vec<String>>()
                    .join(" ")
            })
            .enumerate()
            .map(|(i, line)| format!("{:04X?}0  {}", i, line))
            .collect::<Vec<String>>()
            .join("\n");

        fs::write(output.with_extension("mem"), &mem).unwrap();
    }

    if result.len() > 0 {
        let exit_code = result[0].i32().unwrap_or(1);
        exit(exit_code);
    }
    // No exit code will panic during the validation but for safety
    // if no exit code was given assume there was a problem
    exit(1);
}

fn env_println(arg: i32) {
    println!("{}", arg);
}
