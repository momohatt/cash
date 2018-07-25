open Syntax
open Unix

exception CommandEmpty
exception NotImplemented

type env = string array
type pipe = (file_descr * file_descr)

let histfilename = ".cash_history"
let prompt = "$ "

let run_job (j : job) (env : env) =
  let nproc = List.length j in
  let pipes = List.map (fun _ -> pipe ()) (List.tl j) in
  let rec _setup_pipes (p : pipe list) (procid : int) =
    match p with
    | [] -> ()
    | (pin, pout) :: px ->
      let pipeid = nproc - 1 - List.length p in
      (match procid - pipeid with
       | 1 -> close pout; dup2 pin stdin; close pin
       | 0 -> close pin; dup2 pout stdout; close pout
       | _ -> close pin; close pout);
      _setup_pipes px procid
  in
  let _setup_redir (p : proc) =
    (match p.in_file with
     | Some filename ->
       let fd = openfile filename [O_RDONLY] 0o644 in
       dup2 fd stdin; close fd
     | None -> ());
    (match p.out_file with
     | Some (filename, TRUNC) ->
       let fd = openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
       dup2 fd stdout; close fd
     | Some (filename, APPEND) ->
       let fd = openfile filename [O_WRONLY; O_CREAT; O_APPEND] 0o644 in
       dup2 fd stdout; close fd
     | None -> ())
  in
  let rec _run_job (j : job) (n : int) (cpid : int list) =
    match j with
    | [] ->
      let rec _close_pipe_all (p : pipe list) =
        match p with
        | [] -> ()
        | (pin, pout) :: px -> close pin; close pout; _close_pipe_all px
      in
      let rec _wait_all (cpid : int list) =
        match cpid with
        | [] -> ()
        | pid :: xl -> waitpid [] pid |> ignore; _wait_all xl
      in
      _close_pipe_all pipes;
      _wait_all cpid
    | p :: px ->
      (match fork () with
       | 0 ->
         _setup_pipes pipes n;
         _setup_redir p;
         execvpe p.command p.args env
       | pid ->
         _run_job px (n + 1) (pid :: cpid))
  in _run_job j 0 []

let exec_history () =
  raise NotImplemented

let exec_fg () =
  raise NotImplemented

let exec_bg () =
  raise NotImplemented

let rec read_exec (env : env) =
  match LNoise.linenoise prompt with
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
  openfile histfilename [O_RDWR; O_APPEND; O_CREAT] 0o600 |> ignore;
  LNoise.set_multiline true;
  LNoise.history_load ~filename:histfilename |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;
  let env = environment () in read_exec env

(*
let _ =
  read_print ()
*)

