open Syntax

exception CommandEmpty

let rec read_exec =
  print_string "?- ";
  flush stdout;
  let job_i = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let job = Syntax.job_i_to_job job_i in
  match job with
  | [] -> raise CommandEmpty
  | j :: jx -> (match j.command with
      | "history" -> exec_history
      | "fg" -> exec_fg
      | "bg" -> exec_bg
      | _ -> run_job job)
