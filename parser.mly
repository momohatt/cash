%{
  open Syntax
%}

%token BAR
%token LT GT GTGT
%token <string> STRING

%start toplevel
%type <Syntax.job_i> toplevel
%%

toplevel:
  | command_io              { [$1] }
  | command_io BAR toplevel { $1 :: $3 }
;

/* (command * args) * (in_file * (out_file * out_option)) */
command_io:
  | command LT src GT dest   { ($1, (Some $3, Some ($5, TRUNC))) }
  | command GT dest LT src   { ($1, (Some $5, Some ($3, TRUNC))) }
  | command LT src GTGT dest { ($1, (Some $3, Some ($5, APPEND))) }
  | command GTGT dest LT src { ($1, (Some $5, Some ($3, APPEND))) }
  | command LT src           { ($1, (Some $3, None)) }
  | command GT dest          { ($1, (None, Some ($3, TRUNC))) }
  | command GTGT dest        { ($1, (None, Some ($3, APPEND))) }
  | command                  { ($1, (None, None)) }
;

src:
  | STRING { $1 }
;

dest:
  | STRING { $1 }
;

command:
  | STRING        { ($1, []) }
  | STRING args   { ($1, $2) }
;

args:
  | STRING        { [$1] }
  | STRING args   { $1 :: $2 }
;

