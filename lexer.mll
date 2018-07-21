let space = ' ' | '\t' | '\r' | '\n'
let alphabet = ['a'-'z' 'A'-'Z' '_' '-']
let number = ['0'-'9']
let names = (alphabet | number)*

rule main = parse
| space+      { main lexbuf }
| "|"         { Parser.BAR }
| "<"         { Parser.LT }
| ">"         { Parser.GT }
| ">>"        { Parser.GTGT }
| names as n  { Parser.STRING n }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
