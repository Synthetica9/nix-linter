Usage:

```sh
nix-shell
cd src
runghc Main.hs /etc/nixos/configuration.nix
```

Doesn't have too many checks for now, more are planned in the future.

```The nixlinter program

nixlinter [OPTIONS] [FILES]                                                                                                                                                                    
                                                                                                                                                                                               
Common flags:                                                                                                                                                                                  
  -W --check=ITEM   checks to enable                                                                                                                                                           
  -j --json         Use JSON output                                                                                                                                                            
  -J --json-stream  Use a newline-delimited stream of JSON objects instead of                                                                                                                  
                    a JSON list (implies --json)                                                                                                                                               
  -f --file-list    Read files to process (like xargs)                                                                                                                                         
  -r --recursive    Recursively walk given directories (like find)                                                                                                                             
  -o --out=FILE     File to output to                                                                                                                                                          
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
```
