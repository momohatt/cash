open Syntax

exception CommandEmpty
exception NotImplemented

type env = string array

let run_job (j : job) (env : env) =
  match j with
  | [p] -> Unix.execvpe p.command p.args env
  | _ -> raise NotImplemented

let exec_history () =
  raise NotImplemented

let exec_fg () =
  raise NotImplemented

let exec_bg () =
  raise NotImplemented

let rec read_exec (env : env) =
  print_string "$ ";
  flush stdout;
  let input = read_line () in
  (try
     let job_i = Parser.toplevel Lexer.main (Lexing.from_string input) in
     let job = Syntax.job_i_to_job job_i in
     match job with
     | [] -> raise CommandEmpty
     | j :: jx -> (match j.command with
         | "history" -> exec_history ()
         | "fg" -> exec_fg ()
         | "bg" -> exec_bg ()
         | _ -> run_job job env)
   with
   | Parsing.Parse_error -> print_string "Invalid input."; read_exec env
   | Failure msg -> (match msg with
       | "lexing: empty token" -> ()
       | _ -> print_string (msg ^ "\n") ; read_exec env))


let rec read_print () =
  print_string "$ ";
  flush stdout;
  let input = read_line () in
  let job_i = Parser.toplevel Lexer.main (Lexing.from_string input) in
  let job = Syntax.job_i_to_job job_i in
  (print_job job;
   read_print ())

let _ =
  let env = Unix.environment () in read_exec env

(*
let _ =
  read_print ()
*)

