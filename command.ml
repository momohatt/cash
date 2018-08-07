open Syntax
open Unix
open ExtUnixSpecific

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
  match Array.length arg with
  | 1 -> chdir (getenv "HOME")
  | _ -> (try chdir arg.(1) with
      | Unix_error (eno, _, x) -> raise (MainError (errmsg eno "cd" x)))

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
   | Failure _ -> raise (MainError "No such jobs")
   | Invalid_argument -> raise (MainError "No such jobs"))

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
   | Invalid_argument -> raise (MainError "No such jobs"))

let exec_jobs (jbs : job list) =
  let id = ref 0 in
  let _print j = id := !id + 1; print_job_status j !id;
  in
  List.iter _print jbs;
  flush Pervasives.stdout

