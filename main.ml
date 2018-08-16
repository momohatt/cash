open Syntax
open Utils
open Unix
open ExtUnixSpecific

exception MainError of string
exception NotReached
exception Invalid_argument
exception InvalidCommand

let histfilename = ".cash_history"
let prompt = "$ "
let env = ref [||]


let to_background (j : job) (jbs : job list) =
  tcsetpgrp stdin (getpgid 0);
  print_job_status j ((List.length jbs) + 1);
  flush Pervasives.stdout;
  jbs @ [j]


let wait_foreground_job (j : job) (jbs : job list) =
  let status = List.hd (List.map (fun p -> waitpid [WUNTRACED] p.pid) j.procs) in
  tcsetpgrp stdin (getpid ());
  match status with
  | (_, WSTOPPED n) ->
    j.status <- Stopped;
    to_background j jbs
  | _ -> []


let wait_background_job (jbs : job list) =
  let rec handle_terminated_proc (pid : int) (jbs : job list) =
    let has_pid j = List.exists (fun p -> p.pid = pid) j.procs in
    let j = List.find has_pid jbs in
    j.nalive <- j.nalive - 1;
    match (j.nalive > 0) with
    | false -> remove j jbs
    | true -> jbs
  in
  (try
     let (pid, status) = waitpid [WNOHANG; WUNTRACED] (-1) in
     match pid with
     | 0 -> jbs
     | _ -> handle_terminated_proc pid jbs
   with
   | Not_found -> jbs
   | Unix_error (ECHILD, _, _) -> jbs)


let exec_history () =
  let channel = open_in histfilename in
  let rec _read_hist_print (n : int) =
    (try
       let str = input_line channel in
       Printf.printf "%s : %s\n" (string_of_int n) str;
       _read_hist_print (n + 1)
     with
     | End_of_file -> ())
  in _read_hist_print 0; flush Pervasives.stdout;
  sys_exit 0


let exec_cd (arg : string array) =
  (match Array.length arg with
   | 1 -> chdir (getenv "HOME")
   | _ -> (try chdir arg.(1) with
       | Unix_error (eno, _, x) -> raise (MainError (errmsg eno "cd" x))));
  sys_exit 0


let exec_fg (args : string array) (jbs : job list) =
  (try
     let index = match Array.length args with
       | 1 -> 1
       | _ -> int_of_string args.(1)
     in
     let j = List.nth jbs (index - 1) in
     let newjbs = remove j jbs in
     (match j.status with
      | Stopped ->
        j.status <- Running;
        List.iter (fun p -> kill p.pid Sys.sigcont) j.procs
      | _ -> ());
     j.mode <- Foreground;
     tcsetpgrp stdin j.pgid;
     print_job_status j index; flush Pervasives.stdout;
     wait_foreground_job j newjbs
   with
   | Failure _ -> raise (MainError "No such job")
   | Invalid_argument -> raise (MainError "No such job"))


let exec_bg (args : string array) (jbs : job list) =
  (try
     let index = match Array.length args with
       | 1 -> 1
       | _ -> int_of_string args.(1)
     in
     let j = List.nth jbs (index - 1) in
     (match j.status with
      | Stopped ->
        j.status <- Running;
        List.iter (fun p -> kill p.pid Sys.sigcont) j.procs
      | _ -> ());
     print_job_status j index; flush Pervasives.stdout;
     jbs
   with
   | Failure _ -> raise (MainError "No such job")
   | Invalid_argument -> raise (MainError "No such job"))


let exec_jobs (jbs : job list) =
  let id = ref 0 in
  let _print j = id := !id + 1; print_job_status j !id;
  in
  List.iter _print jbs;
  flush Pervasives.stdout;
  sys_exit 0


let run_job (j : job) (jbs : job list) =
  let nproc = List.length j.procs in
  let pipes = List.map (fun _ -> pipe ()) (List.tl j.procs) in
  let _close_pipe_all (p : pipe list) =
    List.iter (fun (pin, pout) -> close pin; close pout) p
  in
  let rec _run_job (pl : proc list) (n : int) (cpid : int list) (pgid : int) =
    match pl with
    | [] ->
      j.pgid <- pgid;
      sleepf 0.001; (* for setpgid to be effective *)
      _close_pipe_all pipes;
      (match j.mode with
       | Foreground ->
         (match pgid with
          | 0 -> jbs
          | _ -> tcsetpgrp stdin pgid; wait_foreground_job j jbs)
       | Background ->
         to_background j jbs)
    | p :: px ->
      (match fork () with
       | 0 ->
         setup_pipes pipes n nproc;
         setup_redirect p;
         set_signals Sys.Signal_default;
         setpgid 0 pgid;
         (try
            match p.command with
            | "history" -> exec_history ()
            | "cd"      -> exec_cd p.args
            | "jobs"    -> exec_jobs jbs
            | _         -> execvpe p.command p.args !env
          with
          | Unix_error (ENOENT, "execvpe", cmd) ->
            prerr_string ("command not found: " ^ cmd ^ "\n");
            flush Pervasives.stderr;
            kill 0 Sys.sigkill;
            raise InvalidCommand)
       | pid ->
         p.pid <- pid;
         match pgid with
         | 0 -> _run_job px (n + 1) (pid :: cpid) pid
         | _ -> _run_job px (n + 1) (pid :: cpid) pgid)
  in
  (try _run_job j.procs 0 [] 0 with
   | Unix_error (eno, syscall, cmd) -> raise (MainError (errmsg eno syscall cmd)))


let rec read_exec (ojbs : job list) =
  let jbs = wait_background_job ojbs in
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
        | [] -> raise NotReached
        | p :: px -> (match p.command with
            | "exit" -> ()
            | "fg"   -> let newjbs = exec_fg p.args jbs in read_exec newjbs
            | "bg"   -> let newjbs = exec_bg p.args jbs in read_exec newjbs
            | _      -> let newjbs = run_job job jbs in read_exec newjbs)
      with
      | Parsing.Parse_error ->
        prerr_string "Invalid input.\n"; flush Pervasives.stderr; read_exec jbs
      | MainError msg ->
        prerr_string (msg ^ "\n"); flush Pervasives.stderr; read_exec jbs
      | InvalidCommand -> read_exec jbs
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
  set_signals Sys.Signal_ignore;
  env := environment (); read_exec []

(*
let _ =
  read_print ()
*)

