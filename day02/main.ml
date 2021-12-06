(* read_input : unit -> stirng list *)
let read_input () =
  let ic = open_in "./input/day02.txt" in
  let acc = ref [] in
  try
    (while true do
       acc := (input_line ic) :: !acc
    done; !acc)
  with
    End_of_file -> (close_in ic; !acc)

(* calc_position : string list -> int * int *)
let calc_position input =
  List.fold_right
    (fun command (h, d) ->
      match String.split_on_char ' ' command with
          op :: x_str :: [] ->
            let x = int_of_string x_str in
            if op = "forward" then (h + x, d)
            else if op = "down" then (h, d + x)
            else if op = "up" then (h, d - x)
            else assert false
        | _ -> assert false)
    input
    (0, 0)

let main () =
  let input = read_input () in
  let (horiz, depth) = calc_position input in
  (print_int (horiz * depth); print_newline())

let _ = main ()
