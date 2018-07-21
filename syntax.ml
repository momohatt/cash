type write_opt = TRUNC | APPEND

type proc = {
  command  : string;
  args     : string array;
  in_file  : string option;
  out_file : (string * write_opt) option;
}

type job = proc list

(* (command * args) * (in_file * (out_file * out_option)) *)
type proc_i = (string * string list) *
              (string option * ((string * write_opt) option))
type job_i = proc_i list

let proc_i_to_proc (p : proc_i) : proc =
  let ((proc, args), (in_f, out_f)) = p in
  let args_a = Array.of_list args in
  { command = proc; args = args_a; in_file = in_f; out_file = out_f }

let job_i_to_job (j : job_i) : job =
  List.map proc_i_to_proc j


(*** print functions ***)
let string_of_proc (p : proc) =
  let arg_str = Array.fold_left (fun acc s -> acc ^ " " ^ s) "args: " p.args in
  let in_file_str = match p.in_file with
    | Some f -> "input: " ^ f
    | None -> ""
  in
  let out_file_str = match p.out_file with
    | Some (f, _) -> "output: " ^ f
    | None -> ""
  in
  "command: " ^ p.command ^ "\n" ^ arg_str ^ "\n" ^ in_file_str ^ "\n" ^ out_file_str ^ "\n"

let rec string_of_job (j : job) =
  String.concat "" (List.map string_of_proc j)

let print_job (j : job) =
  print_string (string_of_job j)

