let space = ' ' | '\t' | '\r' | '\n'
let alphabet = ['a' - 'z'] | ['A' - 'Z']
let number = ['0' - '9']
let letter = (alphabet | number)
let names = letter*

rule main = parse
| space+        { main lexbuf }
| "|"           { Parser.BAR }
| "<"           { Parser.LT }
| ">"           { Parser.GT }
| ">>"          { Parser.GTGT }
| names as n    { Parser.STRING n }
| _             { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
