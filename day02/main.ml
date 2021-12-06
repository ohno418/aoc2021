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
          "forward" :: [x] -> (h + (int_of_string x), d)
        | "down" :: [x] -> (h, d + (int_of_string x))
        | "up" :: [x] -> (h, d - (int_of_string x))
        | _ -> assert false)
    input
    (0, 0) in
  (print_string "part1: "; print_int (horiz * depth); print_newline())

let print_part2 input =
  let (horiz, depth, _) = List.fold_right
    (fun command (h, d, a) ->
      match String.split_on_char ' ' command with
          "forward" :: [x] -> (h + (int_of_string x), d + a * (int_of_string x), a)
        | "down" :: [x] -> (h, d, a + (int_of_string x))
        | "up" :: [x] -> (h, d, a - (int_of_string x))
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
