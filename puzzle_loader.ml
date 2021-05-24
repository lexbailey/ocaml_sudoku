(* OCaml, please, function composition should be a built in part of the language T_T *)
let after f g = fun x -> f (g x);;

let rec isqrt n =
    if n = 1 then 1
    else let n' = isqrt (n - 1) in
        (n' + (n / n')) / 2
    ;;

let puzzle_from_file filename =
    let i = open_in filename in
    let s = really_input_string i (in_channel_length i) in
    let l1 = List.of_seq (String.to_seq s) in
    let l2 = List.filter (after (not) (String.contains "-| \n\r")) l1 in
    let cell_strings = String.split_on_char ',' (String.of_seq (List.to_seq l2)) in
    let grid = List.map (fun n -> match n with
        | "" -> None
        | x -> Some(int_of_string x)
    ) cell_strings in
    let len = List.length grid in
    let rw = isqrt (isqrt len) in
    let valid = (rw*rw*rw*rw) == len in
    if valid then
        (rw, grid)
    else
        (
        Printf.printf "Input puzzle from file '%s' is not valid. It contains %d cells, which is not a fourth power.\n" filename len;
        exit 1;
        )
    ;;

(*let puzzle = puzzle_from_file "example_input_9x9" ;;*)
