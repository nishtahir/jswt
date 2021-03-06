mod env;

#[macro_use]
extern crate clap;

use clap::Arg;
use jswt_ast::Ast;
use jswt_ast_lowering::AstLowering;
use jswt_ast_serializer::AstSerializer;
use std::cell::Cell;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use wasmer::{imports, Function, Instance, MemoryView, Module as WasmerModule, Store};

use jswt_codegen::CodeGenerator;
use jswt_errors::{print_parser_error, print_semantic_error, print_tokenizer_error};
use jswt_parser::Parser;
use jswt_semantics::SemanticAnalyzer;
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
        .arg(
            Arg::with_name("minified")
                .long("minified")
                .required(false)
                .takes_value(false)
                .help("Minify WAST output"),
        )
        .arg(
            Arg::with_name("no-std")
                .long("no-std")
                .required(false)
                .takes_value(false)
                .help("Do not include the runtime and stdlib"),
        )
        .arg(
            Arg::with_name("runtime-path")
                .long("runtime-path")
                .required(false)
                .takes_value(true)
                .help("Path to runtime sources"),
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

    let runtime = if matches.is_present("no-std") {
        // Don't include the runtime even if the path is provided
        None
    } else {
        // If a runtime path isn't provided use the default path
        matches
            .value_of("runtime-path")
            .or(Some("./runtime/rt.jswt"))
            .map(fs::canonicalize)
            .map(Result::unwrap)
    };

    let ast = compile_module(&input, &output, runtime.as_ref());
    let mut code_gen = CodeGenerator::default();
    let module = code_gen.generate_module(&ast);
    // Write generated wasm AST for debugging
    let wast = module.as_wat(matches.is_present("minified"));

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
            "println" => Function::new_native(&store, env::println),
            "exit" => Function::new_native(&store, env::exit),
            "assertEqual" => Function::new_native(&store, env::assert_equal)
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

fn compile_module(input: &Path, output: &Path, runtime: Option<&PathBuf>) -> Ast {
    // Let binding to prevent the ref being dropped before getting passed to the tokenizer
    let mut tokenizer = Tokenizer::new();
    // Sources root for user defined sources is the current directory
    // from where the compiler is being invoked.
    tokenizer.set_sources_root(Some(&std::env::current_dir().unwrap()));
    tokenizer.set_module_prefix(Some("module".to_string()));
    tokenizer.enqueue_source_file(input);

    if let Some(runtime) = runtime {
        // Sources at the top of the source stack will be resolved first
        // This has implications in the parsing and semantic analysis process
        // As errors related to redefinitions and shadowing
        // should point to user defined sources
        tokenizer.set_sources_root(runtime.parent().map(Path::to_path_buf).as_ref());
        tokenizer.set_module_prefix(Some("runtime".to_string()));
        tokenizer.enqueue_source_file(runtime);
    }

    let mut parser = Parser::new(&mut tokenizer);
    let mut ast = parser.parse();

    // Write AST for debugging
    fs::write(output.with_extension("ast"), format!("{:#?}", ast)).unwrap();

    let mut has_errors = false;

    // Report errors
    for error in parser.tokenizer_errors() {
        has_errors = true;
        print_tokenizer_error(&error);
    }

    for error in parser.parse_errors() {
        has_errors = true;
        print_parser_error(&error);
    }

    // Semantic analytis pass
    let mut analyzer = SemanticAnalyzer::default();
    let semantic_errors = analyzer.analyze(&mut ast);

    for error in semantic_errors {
        has_errors = true;
        print_semantic_error(&error);
    }

    if has_errors {
        exit(1);
    }

    let mut lowering = AstLowering::new(&mut analyzer.bindings_table, &mut analyzer.symbol_table);
    lowering.desugar(&mut ast);

    fs::write(output.with_extension("lowered.ast"), format!("{:#?}", ast)).unwrap();

    let mut serializer = AstSerializer::default();
    let content = serializer.serialze(&ast);
    fs::write(output.with_extension("lowered.jswt"), content).unwrap();

    if has_errors {
        exit(1);
    }

    ast
}

#[cfg(test)]
mod test {
    use assert_cmd::prelude::{CommandCargoExt, OutputAssertExt};
    use jswt_assert::{assert_snapshot, assert_str_eq};
    use regex::Regex;
    use std::{borrow::Cow, process::Command};

    #[test]
    fn test_compile_and_execute_variables_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        cmd.arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/variables.jswt")
            .assert()
            .success();
    }

    #[test]
    fn test_compile_and_execute_loops_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/loops.jswt")
            .assert()
            .success();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_str_eq!(stdout, "45\n");
    }

    #[test]
    fn test_compile_and_execute_arithmetics_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/math.jswt")
            .assert()
            .success();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_str_eq!(stdout, "7\n1\n12\n1\n");
    }

    #[test]
    #[ignore]
    fn test_compile_and_execute_arrays_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/arrays.jswt")
            .assert()
            .success();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_str_eq!(stdout, "99\n2\n3\n4\n");
    }

    #[test]
    fn test_variable_not_found_semantic_error() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("./test/variable-not-found.jswt")
            .assert()
            .failure();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_snapshot!(redact_paths(stdout));
    }

    /// Simple redaction of paths that will be generated by errors
    /// in the crate to keeps tests that may run on different systems
    /// from failing.
    fn redact_paths(value: &str) -> Cow<str> {
        let re = Regex::new(r"/[\w/]+/jswt/jswt-compiler/").unwrap();
        re.replace_all(value, "[redacted]/jswt/jswt-compiler/")
    }
}
