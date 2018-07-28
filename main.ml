open Syntax
open Unix
open ExtUnixSpecific

exception CommandEmpty
exception NotImplemented

type pipe = (file_descr * file_descr)

let histfilename = ".cash_history"
let prompt = "$ "
let env = ref [||]

let print_error eno f x =
  print_string (f ^ ": " ^ (error_message eno) ^ ": " ^ x ^ "\n");
  flush Pervasives.stdout

let to_background (j : job) (jbs : job list) =
  tcsetpgrp stdin (getpgid 0);
  Printf.printf "[%d] %d\n" ((List.length jbs) + 1) j.pgid;
  flush Pervasives.stdout;
  j :: jbs

let wait_foreground_job (j : job) (jbs : job list) =
  let status = List.hd (List.map (fun p -> waitpid [WUNTRACED] p.pid) j.procs) in
  tcsetpgrp stdin (getpid ());
  match status with
  | (_, WSTOPPED _) ->
    j.status <- Stopping;
    to_background j jbs
  | _ -> jbs

let run_job (j : job) (jbs : job list) =
  let nproc = List.length j.procs in
  let pipes = List.map (fun _ -> pipe ()) (List.tl j.procs) in
  let rec _run_job (pl : proc list) (n : int) (cpid : int list) (pgid : int) =
    match pl with
    | [] ->
      let rec _close_pipe_all (p : pipe list) =
        List.iter (fun (pin, pout) -> close pin; close pout) p
      in
      let rec _wait_all (cpid : int list) =
        List.iter (fun pid -> waitpid [] pid |> ignore) cpid
      in
      j.pgid <- pgid;
      sleepf 0.0005; (* for setpgid to be effective *)
      _close_pipe_all pipes;
      (match j.mode with
       | Foreground ->
         tcsetpgrp stdin pgid;
         wait_foreground_job j jbs
       | Background ->
         to_background j jbs)
    | p :: px ->
      (match fork () with
       | 0 ->
         Utils.setup_pipes pipes n nproc;
         Utils.setup_redirect p;
         Utils.setup_signals Sys.Signal_default;
         setpgid 0 pgid;
         execvpe p.command p.args !env
       | pid ->
         p.pid <- pid;
         match pgid with
         | 0 -> _run_job px (n + 1) (pid :: cpid) pid
         | _ -> _run_job px (n + 1) (pid :: cpid) pgid)
  in _run_job j.procs 0 [] 0

let exec_history () =
  let channel = open_in histfilename in
  let rec _read_hist_print (n : int) =
    (try
       let str = input_line channel in
       print_string ((string_of_int n) ^ " : " ^ str ^ "\n");
       _read_hist_print (n + 1)
     with
     | End_of_file -> ())
  in _read_hist_print 0; flush Pervasives.stdout

let exec_cd arg =
  (try
     chdir arg
   with
   | Unix_error (eno, _, x) -> print_error eno "cd" x)

let exec_fg (jbs : job list) =
  raise NotImplemented

let exec_bg (jbs : job list) =
  raise NotImplemented

let exec_jobs (jbs : job list) =
  let id = ref 0 in
  let _print j = id := !id + 1; Printf.printf "[%d] %s\n" !id j.command in
  List.iter _print jbs;
  flush Pervasives.stdout

let rec read_exec (jbs : job list) =
  match LNoise.linenoise prompt with
  | None -> () (* terminating the shell *)
  | Some "" -> read_exec jbs
  | Some input ->
    (LNoise.history_add input |> ignore;
     LNoise.history_save histfilename |> ignore;
     (try
        let job_i = Parser.toplevel Lexer.main (Lexing.from_string input) in
        let job = Syntax.job_i_to_job job_i in
        job.command <- input;
        match job.procs with
        | [] -> raise CommandEmpty
        | j :: jx -> (match j.command with
            | "exit" -> ()
            | "history" -> exec_history (); read_exec jbs
            | "cd" -> exec_cd j.args.(1); read_exec jbs
            | "fg" -> exec_fg jbs
            | "bg" -> exec_bg jbs
            | "jobs" -> exec_jobs jbs; read_exec jbs
            | _ -> let newjbs = run_job job jbs in read_exec newjbs)
      with
      | Parsing.Parse_error -> print_string "Invalid input.\n"; flush Pervasives.stdout; read_exec jbs
      | End_of_file -> ()))


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
  Utils.setup_signals Sys.Signal_ignore;
  env := environment (); read_exec []

(*
let _ =
  read_print ()
*)

