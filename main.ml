open Syntax
open Unix

exception CommandEmpty
exception NotImplemented

type env = string array

let histfilename = ".history"

let rec run_job (j : job) (env : env) =
  match j with
  | [] -> let _ = wait () in ()
  | p :: px ->
    (match fork () with
     | 0 -> execvpe p.command p.args env
     | _ -> run_job px env)

let exec_history () =
  raise NotImplemented

let exec_fg () =
  raise NotImplemented

let exec_bg () =
  raise NotImplemented

let rec read_exec (env : env) =
  match LNoise.linenoise "$ " with
  | None -> () (* terminating the shell *)
  | Some input ->
    LNoise.history_add input |> ignore;
    LNoise.history_save histfilename |> ignore;
    (try
       let job_i = Parser.toplevel Lexer.main (Lexing.from_string input) in
       let job = Syntax.job_i_to_job job_i in
       match job with
       | [] -> raise CommandEmpty
       | j :: jx -> (match j.command with
           | "exit" -> ()
           | "history" -> exec_history ()
           | "fg" -> exec_fg ()
           | "bg" -> exec_bg ()
           | _ -> run_job job env; read_exec env)
     with
     | Parsing.Parse_error -> print_string "Invalid input."; read_exec env
     | End_of_file -> ())


let rec read_print () =
  print_string "$ ";
  flush Pervasives.stdout;
  let input = read_line () in
  let job_i = Parser.toplevel Lexer.main (Lexing.from_string input) in
  let job = Syntax.job_i_to_job job_i in
  (print_job job;
   read_print ())

let _ =
  open_out_gen [Open_creat; Open_append] 644 histfilename |> ignore;
  LNoise.set_multiline true;
  LNoise.history_load ~filename:histfilename |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;
  let env = environment () in read_exec env

(*
let _ =
  read_print ()
*)

