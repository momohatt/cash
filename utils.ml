open Unix
open Syntax

type pipe = (file_descr * file_descr)

let setup_signals (s : Sys.signal_behavior) =
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
