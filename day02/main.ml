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

let print_part1 input =
  let (horiz, depth) = List.fold_right
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
    (0, 0) in
  (print_string "part1: "; print_int (horiz * depth); print_newline())

let print_part2 input =
  let (horiz, depth, _) = List.fold_right
    (fun command (h, d, a) ->
      match String.split_on_char ' ' command with
          op :: x_str :: [] ->
            let x = int_of_string x_str in
            if op = "forward" then (h + x, d + a * x, a)
            else if op = "down" then (h, d, a + x)
            else if op = "up" then (h, d, a - x)
            else assert false
        | _ -> assert false)
    input
    (* (horizontal, depth, aim) *)
    (0, 0, 0) in
  (print_string "part2: "; print_int (horiz * depth); print_newline())

let main () =
  let input = read_input () in
  (print_part1 input;
   print_part2 input)

let _ = main ()
