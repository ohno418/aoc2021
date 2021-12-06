(* Read from ./input/day01.txt. *)
(* read_input : unit -> int list *)
let read_input () =
  let acc = ref [] in
  let ic = open_in "./input/day01.txt" in
  try
    (while true do
       acc := int_of_string (input_line ic) :: !acc
     done; !acc)
  with
    End_of_file -> (close_in ic; !acc)

(* part1 : int list -> int *)
let part1 input_list =
  (* `cnt` is an accumulator. *)
  let rec count_up lst cnt =
    match lst with
        a :: b :: rest ->
          if a > b then count_up (b :: rest) cnt + 1
                   else count_up (b :: rest) cnt
      | _ -> cnt in
  count_up input_list 0

(* part2 : int list -> int *)
let part2 input_list =
  let rec count_up lst cnt =
    match lst with
        a :: b :: c :: d :: rest ->
          if (a + b + c) > (b + c + d) then count_up (b :: c :: d :: rest) (cnt + 1)
                                       else count_up (b :: c :: d :: rest) cnt
      | _ -> cnt in
  count_up input_list 0

let main () =
  let input_list = read_input () in
  (print_string "part1: "; print_int (part1 input_list); print_newline ();
   print_string "part2: "; print_int (part2 input_list); print_newline ())

let _ = main ()
