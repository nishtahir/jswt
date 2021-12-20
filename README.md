<div>
<h1 align="center">JSWT</h1>
<p align="center">
    <a href="https://github.com/nishtahir/jswt/actions/workflows/rust.yml" alt="Build">
        <img src="https://github.com/nishtahir/jswt/actions/workflows/rust.yml/badge.svg" /></a>
    <a href="https://codecov.io/gh/nishtahir/jswt" alt="Coverage">
        <img src="https://codecov.io/gh/nishtahir/jswt/branch/master/graph/badge.svg?token=VL6CT8HU4U" /></a>
</p>
</div>

A JavaScript/TypeScript like language that compiles to web assembly.

## CLI

```
USAGE:
    jswt [FLAGS] [OPTIONS] <file>

FLAGS:
    -h, --help        Prints help information
        --log-mem     Log the memory layout after execution
        --minified    Minify WAST output
        --no-std      Do not include the runtime and stdlib
    -V, --version     Prints version information

OPTIONS:
    -o <output>                          Write output to file
        --runtime-path <runtime-path>    Path to runtime sources

ARGS:
    <file>    Input file to begin compiling
```

## Resources

* [Official Web Assembly Specification](https://webassembly.github.io/spec/core/)
* [Web Assembly Reference Manual](https://github.com/sunfishcode/wasm-reference-manual)
* [Writing WebAssembly By Hand](https://blog.scottlogic.com/2018/04/26/webassembly-by-hand.html)