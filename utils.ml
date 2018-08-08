open Syntax
open Unix

type pipe = (file_descr * file_descr)

let print_job_status (j : job) (i : int) =
  match j.status with
  | Running    -> Printf.printf "[%d] (pid: %d) Running   : %s\n" i j.pgid j.command
  | Stopped    -> Printf.printf "[%d] (pid: %d) Stopped   : %s\n" i j.pgid j.command
  | Terminated -> Printf.printf "[%d] (pid: %d) Terminated: %s\n" i j.pgid j.command


let fprint_job_status (j : job) (i : int) (fd : out_channel) =
  match j.status with
  | Running    ->
    Printf.fprintf fd "[%d] (pid: %d) Running   : %s\n" i j.pgid j.command
  | Stopped    ->
    Printf.fprintf fd "[%d] (pid: %d) Stopped   : %s\n" i j.pgid j.command
  | Terminated ->
    Printf.fprintf fd "[%d] (pid: %d) Terminated: %s\n" i j.pgid j.command


let errmsg eno f arg =
  Printf.sprintf "%s: %s: %s" (error_message eno) f arg


let set_signals (s : Sys.signal_behavior) =
  Sys.set_signal Sys.sigint  s;
  Sys.set_signal Sys.sigtstp s;
  Sys.set_signal Sys.sigttou s;
  Sys.set_signal Sys.sigttin s;
  Sys.set_signal Sys.sigquit s


let rec setup_pipes (p : pipe list) (procid : int) (nproc : int) =
  match p with
  | [] -> ()
  | (pin, pout) :: px ->
    let pipeid = nproc - 1 - List.length p in
    (match procid - pipeid with
     | 1 -> close pout; dup2 pin stdin; close pin
     | 0 -> close pin; dup2 pout stdout; close pout
     | _ -> close pin; close pout);
    setup_pipes px procid nproc


let setup_pipes_nodup (p : pipe list) (procid : int) (nproc : int) =
  let rec _inner (p : pipe list) fdin fdout =
    match p with
    | [] -> (fdin, fdout)
    | (pin, pout) :: px ->
      let pipeid = nproc - 1 - List.length p in
      (match procid - pipeid with
       | 1 -> close pout; _inner px (Some pin) fdout
       | 0 -> close pin; _inner px fdin (Some pout)
       | _ -> close pin; close pout; _inner px fdin fdout)
  in _inner p None None


let setup_redirect (p : proc) =
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


let rec remove (a : 'a) (l : 'a list) =
  match l with
  | [] -> []
  | x :: xl -> match (a = x) with
    | true -> xl
    | false -> x :: (remove a xl)

