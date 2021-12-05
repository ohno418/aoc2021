(*
$ ocamlopt -o main day01_1.ml
$ ./main < sample.txt
*)

let main () =
  (* read_from_stdin : unit -> int list *)
  let read_from_stdin () =
    let acc = ref [] in
    try
      (while true do
         acc := int_of_string (input_line stdin) :: !acc
       done; [])
    with
      End_of_file -> !acc in
  let depth_list = read_from_stdin () in
  (* `cnt` is an accumulator. *)
  let rec count_up lst cnt =
    match lst with
        [] -> assert false
      | a :: [] -> cnt
      | b :: a :: rest ->
          if a < b then count_up (a :: rest) cnt + 1
                   else count_up (a :: rest) cnt in
  let cnt = count_up depth_list 0 in
  (print_int cnt;
   print_newline ())

let _ = main ()
