type write_opt = TRUNC | APPEND
type job_status = Running | Stopped | Terminated
type job_mode   = Foreground | Background

type proc = {
  mutable pid : int;
  command     : string;
  args        : string array;
  in_file     : string option;
  out_file    : (string * write_opt) option
}

type job = {
  procs           : proc list;
  mutable status  : job_status;
  mutable mode    : job_mode;
  mutable pgid    : int;
  mutable command : string;
  mutable nalive  : int;       (* number of processes that haven't terminated *)
}

(* (command * args) * (in_file * (out_file * out_option)) *)
type proc_i = (string * string list) *
              (string option * ((string * write_opt) option))
type job_i = (proc_i list * job_mode)

let proc_i_to_proc (p : proc_i) : proc =
  let ((cmd, args), (in_f, out_f)) = p in
  (* include the command string to the args_a *)
  let args_a = Array.of_list (cmd :: args) in
  { pid = 0; command = cmd; args = args_a; in_file = in_f; out_file = out_f }

let job_i_to_job (j : job_i) : job =
  { procs   = List.map proc_i_to_proc (fst j);
    status  = Running;
    mode    = (snd j);
    pgid    = 0;
    command = "";
    nalive  = List.length (fst j); }


(*** print functions ***)
let string_of_proc (p : proc) =
  let arg_str = Array.fold_left (fun acc s -> acc ^ " " ^ s) "args: " p.args in
  let in_file_str = match p.in_file with
    | Some f -> "input: " ^ f ^ "  "
    | None -> ""
  in
  let out_file_str = match p.out_file with
    | Some (f, _) -> "output: " ^ f ^ "  "
    | None -> ""
  in
  "command: " ^ p.command ^ "\n" ^ arg_str ^ "\n" ^ in_file_str ^ out_file_str

let rec string_of_job (j : job) =
  String.concat "" (List.map string_of_proc j.procs)

let string_of_proc_i (p : proc_i) =
  let ((cmd, args), (in_file, out_file)) = p in
  let arg_str = String.concat "" args in
  let in_file_str = match in_file with
    | Some f -> "input: " ^ f ^ "\n"
    | None -> ""
  in
  let out_file_str = match out_file with
    | Some (f, _) -> "output: " ^ f ^ "\n"
    | None -> ""
  in
  "command: " ^ cmd ^ "\n" ^ arg_str ^ "\n" ^ in_file_str ^ out_file_str

let rec string_of_job_i (j : job_i) =
  String.concat "" (List.map string_of_proc_i (fst j))

let print_proc (p : proc) = print_string (string_of_proc p)
let print_proc_i (p : proc_i) = print_string (string_of_proc_i p)
let print_job (j : job) = print_string (string_of_job j)
let print_job_i (j : job_i) = print_string (string_of_job_i j)

