(* Read from ./input/<file_name>. *)
(* read_input : unit -> string list *)
let read_input file_name =
  let ic = open_in ("./input/" ^ file_name) in
  let acc = ref [] in
  try
    (while true do
       acc := input_line ic :: !acc
     done; !acc)
  with
    End_of_file -> (close_in ic; !acc)
