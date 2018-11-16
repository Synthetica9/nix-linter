Usage:

```sh
nix-shell
cd src
runghc Main.hs /etc/nixos/configuration.nix
```

Doesn't have too many checks for now, more are planned in the future.

```
The nixlinter program

nixlinter [OPTIONS] [FILES]

Common flags:
  -c --check=OFFENSECATEGORY    checks to enable
  -n --nocheck=OFFENSECATEGORY  checks to disable
  -j --json                     Use JSON output
  -J --json-stream              Use a newline-delimited stream of JSON
                                objects instead of a JSON list (implies --json)
  -f --file-list                Read files to process (like xargs)
  -w --walk                     Walk given directories (like find)
  -o --out=FILE                 File to output to
  -? --help                     Display help message
  -V --version                  Print version information
  -v --verbose                  Loud verbosity
  -q --quiet                    Quiet verbosity

Available checks:
    UnusedLetBind
    UnusedArg
    EmptyInherit
    UnneededRec
    ListLiteralConcat
    SetLiteralUpdate
    UpdateEmptySet
    UnneededAntiquote
    NegateAtom
    EtaReduce
    FreeLetInFunc
    LetInInheritRecset
    DIYInherit
    EmptyLet
    UnfortunateArgName
```
