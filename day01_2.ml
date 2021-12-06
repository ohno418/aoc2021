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
  (* `cnt` is accumulator. *)
  let rec count_up lst cnt =
    match lst with
        a :: b :: c :: d :: rest ->
          if (a + b + c) > (b + c + d) then count_up (b :: c :: d :: rest) (cnt + 1)
                                       else count_up (b :: c :: d :: rest) cnt
      | _ -> cnt in
  (print_int (count_up depth_list 0);
   print_newline ())

let _ = main ()
