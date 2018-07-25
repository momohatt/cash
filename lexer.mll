let space = ' ' | '\t' | '\r'
let alphabet = ['a'-'z' 'A'-'Z' '_' '-' '.' '/']
let number = ['0'-'9']
let names = (alphabet | number)+

rule main = parse
| space+      { main lexbuf }
| "|"         { Parser.BAR }
| "<"         { Parser.LT }
| ">"         { Parser.GT }
| ">>"        { Parser.GTGT }
| "&"         { Parser.AND }
| eof         { Parser.EOF }
| names as n  { Parser.ID n }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf) }
