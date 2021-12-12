#use "utils/read_input.ml"

(* aggregate_with_str : string -> (int * int) list -> int -> (int * int) list *)
let rec aggregate_with_str str acc idx =
  match acc with
  | [] -> acc
  | a :: rest ->
      (match str.[idx] with
       | '0' -> (match a with (x, y) -> (x + 1, y))
       | '1' -> (match a with (x, y) -> (x, y + 1))
       | _ -> assert false) :: aggregate_with_str str rest (idx + 1)

(* aggregate : string list -> (int * int) list -> (int * int) list *)
let rec aggregate input acc =
  match input with
  | [] -> acc
  | str :: rest ->
      aggregate_with_str str acc 0
      |> aggregate rest

(* Convert binary character list into decimal number.
 * (`sum` is an accumulator.)
 *
 * e.g.
 *   binary_list_to_dec ['0'; '1'; '0'; '1'] = 5 *)
(* binary_list_to_dec : char list -> int -> int *)
let rec binary_list_to_dec ?(sum=0) binary_list =
  match binary_list with
  | [] -> sum
  | bin :: rest ->
      match bin with
      | '0' -> binary_list_to_dec ~sum rest
      | '1' ->
          binary_list_to_dec
            ~sum:(sum + int_of_float (2. ** (float_of_int (List.length binary_list - 1))))
            rest
      | _ -> assert false

let main () =
  let input = read_input "day03.txt" in
  (* List of '0' or '1'. *)
  let bin_list : char list =
    let strlen =
      match input with
      | str :: rest -> String.length str
      | [] -> assert false in
    List.init (strlen) (fun _ -> (0, 0))
    |> aggregate input
    |> List.map
         (fun tup ->
           match tup with
           | (a, b) ->
               if a > b then '0' else '1') in
  let invert_bin_list : char list =
    List.map (fun c -> if c = '0' then '1' else '0') bin_list in
  (binary_list_to_dec bin_list) * (binary_list_to_dec invert_bin_list)

let _ = main ()
