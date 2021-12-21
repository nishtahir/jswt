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

## Architecture

Compilation takes place in phases described in the following chart. Each of these phases roughly takes place in a module of a similar name.

[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggTFJcbiAgICBBW0lucHV0Lmpzd3RdIC0tPiBCXG4gICAgICAgIHN1YmdyYXBoIFBhcnNpbmdcbiAgICAgICAgICAgIEIoVG9rZW5pemVyKSAtLT4gQyhQYXJzZXIpXG4gICAgICAgIGVuZFxuICAgIEMgLS0-IERbQVNUXVxuICAgIEQgLS0-IEVcbiAgICAgICAgc3ViZ3JhcGggU2VtYW50aWMgQW5hbHlzaXNcbiAgICAgICAgICAgIEUoR2xvYmFsIFN5bWJvbCBSZXNvbHV0aW9uKSAtLT4gRihMb2NhbCBTeW1ib2wgUmVzb2x1dGlvbilcbiAgICAgICAgICAgICAtLT4gRyhUeXBlIENoZWNraW5nKVxuICAgICAgICBlbmRcbiAgICBHIC0tPiBIKENvZGUgR2VuZXJhdGlvbilcbiAgICAgICAgc3ViZ3JhcGggQ29kZSBHZW5lcmF0aW9uXG4gICAgICAgICAgICBIKFdBU1QgR2VuZXJhdGlvbilcbiAgICAgICAgZW5kIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQifSwidXBkYXRlRWRpdG9yIjpmYWxzZSwiYXV0b1N5bmMiOnRydWUsInVwZGF0ZURpYWdyYW0iOmZhbHNlfQ)](https://mermaid.live/edit/#eyJjb2RlIjoiZ3JhcGggTFJcbiAgICBBW0lucHV0Lmpzd3RdIC0tPiBCXG4gICAgICAgIHN1YmdyYXBoIFBhcnNpbmdcbiAgICAgICAgICAgIEIoVG9rZW5pemVyKSAtLT4gQyhQYXJzZXIpXG4gICAgICAgIGVuZFxuICAgIEMgLS0-IERbQVNUXVxuICAgIEQgLS0-IEVcbiAgICAgICAgc3ViZ3JhcGggU2VtYW50aWMgQW5hbHlzaXNcbiAgICAgICAgICAgIEUoR2xvYmFsIFN5bWJvbCBSZXNvbHV0aW9uKSAtLT4gRihMb2NhbCBTeW1ib2wgUmVzb2x1dGlvbilcbiAgICAgICAgICAgICAtLT4gRyhUeXBlIENoZWNraW5nKVxuICAgICAgICBlbmRcbiAgICBHIC0tPiBIKENvZGUgR2VuZXJhdGlvbilcbiAgICAgICAgc3ViZ3JhcGggQ29kZSBHZW5lcmF0aW9uXG4gICAgICAgICAgICBIKFdBU1QgR2VuZXJhdGlvbilcbiAgICAgICAgZW5kIiwibWVybWFpZCI6IntcbiAgXCJ0aGVtZVwiOiBcImRlZmF1bHRcIlxufSIsInVwZGF0ZUVkaXRvciI6ZmFsc2UsImF1dG9TeW5jIjp0cnVlLCJ1cGRhdGVEaWFncmFtIjpmYWxzZX0)

[Wasmer](https://wasmer.io/) is used as the WASM execution engine.

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
* [Wasmer](https://wasmer.io/)