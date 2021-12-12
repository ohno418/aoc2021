#use "utils/read_input.ml"

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
  let input_list =
    read_input "day01.txt"
    |> List.map (fun str -> int_of_string str) in
  (print_string "part1: "; print_int (part1 input_list); print_newline ();
   print_string "part2: "; print_int (part2 input_list); print_newline ())

let _ = main ()
