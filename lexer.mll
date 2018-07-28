let space = ' ' | '\t' | '\r'
let alphabet = ['a'-'z' 'A'-'Z' '_' '-' '.' '/' '$' ',' '&']
let number = ['0'-'9']
let names = (alphabet | number)+

rule main = parse
| space+      { main lexbuf }
| "|"         { Parser.BAR }
| "<"         { Parser.LT }
| ">"         { Parser.GT }
| ">>"        { Parser.GTGT }
| "&"         { Parser.AND }
| "\"" ([^'"' '\\'] | '\\'['\x00'-'\xff'])+ "\"" {
    let lexeme = Bytes.sub_string
      lexbuf.lex_buffer
      (lexbuf.lex_start_pos + 1)
      (lexbuf.lex_curr_pos - lexbuf.lex_start_pos - 2) in
    Parser.STRING(Scanf.unescaped lexeme)
  }
| "'" ([^'\'' '\\'] | '\\'['\x00'-'\xff'])+ "'" {
    let lexeme = Bytes.sub_string
      lexbuf.lex_buffer
      (lexbuf.lex_start_pos + 1)
      (lexbuf.lex_curr_pos - lexbuf.lex_start_pos - 2) in
    Parser.STRING(Scanf.unescaped lexeme)
  }
| eof         { Parser.EOF }
| names as n  { Parser.ID n }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf) }
