# `nix-linter`

[![Build Status](https://travis-ci.org/Synthetica9/nix-linter.svg?branch=master)](https://travis-ci.org/Synthetica9/nix-linter)

`nix-linter` is a program to check for several common mistakes or stylistic
errors in Nix expressions, such as unused arguments, empty let blocks,
etcetera.

## Usage

First, setup cachix:

```sh
cachix use nix-linter
```

Then, you can run the application with:

```sh
$(nix-build -A nix-linter)/bin/nix-linter --help

```

```
The nix-linter program

nix-linter [OPTIONS] [FILES]

Common flags:
  -W --check=ITEM   checks to enable
  -j --json         Use JSON output
  -J --json-stream  Use a newline-delimited stream of JSON objects instead of
                    a JSON list (implies --json)
  -r --recursive    Recursively walk given directories (like find)
  -o --out[=FILE]   File to output to
  -? --help         Display help message
  -V --version      Print version information
  -v --verbose      Loud verbosity
  -q --quiet        Quiet verbosity

Available checks:
    DIYInherit
    EmptyInherit
    EmptyLet
    EtaReduce
    FreeLetInFunc
    LetInInheritRecset
    ListLiteralConcat
    NegateAtom
    SequentialLet
    SetLiteralUpdate
    UnfortunateArgName
    UnneededAntiquote
    UnneededRec
    UnusedArg
    UnusedLetBind
    UpdateEmptySet
    AlphabeticalArgs (disabled by default)
    AlphabeticalBindings (disabled by default)
    BetaReduction (disabled by default)
    EmptyVariadicParamSet (disabled by default)

```
