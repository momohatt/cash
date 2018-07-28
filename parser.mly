%{
  open Syntax
%}

%token BAR LT GT GTGT AND EOF
%token <string> STRING
%token <string> ID

%start toplevel
%type <Syntax.job_i> toplevel
%%

toplevel:
  | commands EOF     { ($1, Foreground) }
  | commands AND EOF { ($1, Background) }
;

commands:
  | command_io              { [$1] }
  | command_io BAR commands { $1 :: $3 }
;

/* (command * args) * (in_file * (out_file * out_option)) */
command_io:
  | command LT ID GT ID   { ($1, (Some $3, Some ($5, TRUNC))) }
  | command GT ID LT ID   { ($1, (Some $5, Some ($3, TRUNC))) }
  | command LT ID GTGT ID { ($1, (Some $3, Some ($5, APPEND))) }
  | command GTGT ID LT ID { ($1, (Some $5, Some ($3, APPEND))) }
  | command LT ID         { ($1, (Some $3, None)) }
  | command GT ID         { ($1, (None, Some ($3, TRUNC))) }
  | command GTGT ID       { ($1, (None, Some ($3, APPEND))) }
  | command               { ($1, (None, None)) }
;

command:
  | ID        { ($1, []) }
  | ID args   { ($1, $2) }
;

args:
  | atom        { [$1] }
  | atom args   { $1 :: $2 }
;

atom:
  | ID       { $1 }
  | STRING   { $1 }
;


