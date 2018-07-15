type write_opt = TRUNC | APPEND

type proc = {
  command  : string;
  args     : string list;
  in_file  : string option;
  out_file : (string * write_opt) option;
}

type job = proc list

(* (command * args) * (in_file * (out_file * out_option)) *)
type proc_i = (string * string list) *
             (string option * ((string * write_opt) option))
type job_i = proc_i list

let proc_i_to_proc (c : proc_i) : proc =
  let ((proc, args), (in_f, out_f)) = c in
  { command = proc; args = args; in_file = in_f; out_file = out_f }

let job_i_to_job (j : job_i) : job =
  List.map proc_i_to_proc j

(*** print functions ***)
