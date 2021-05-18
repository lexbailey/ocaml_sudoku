open Types;;
open Printutils;;

let rw = 3 ;; (* root width. 2 means you get a 4*4 board, 3 means 9*9 *)

let w = rw * rw ;; (* width *)
let n_cells = w * w ;; (* total number of cells on the board *)

let pp = Printf.printf ;;

(* Set of all numbers that can be placed in a cell *)
let cell_domain = IntSet.of_list (List.init w (fun x -> x+1)) ;;

(*
let test_board =
    [ None; None; None; Some(1)
    ; Some(1); Some(2); None; None
    ; None; None; None; None
    ; None; None; None; None
    ] ;;
*)


let test_board =
    [ None;    None;    None;      None;    None;    None;      Some(6); Some(1); Some(9);
      None;    Some(2); Some(3);   None;    Some(9); None;      None;    None;    None;
      None;    None;    None;      Some(1); Some(4); None;      None;    Some(2); None;

      None;    None;    Some(9);   None;    None;    None;      None;    None;    None;
      Some(7); None;    Some(8);   None;    None;    Some(3);   None;    None;    None;
      None;    None;    None;      None;    None;    Some(5);   Some(3); Some(4); None;

      None;    None;    None;      None;    None;    None;      Some(4); Some(6); Some(7);
      Some(8); Some(3); None;      None;    None;    None;      None;    None;    None;
      None;    None;    None;      Some(6); Some(1); Some(2);   None;    None;    None;
    ] ;;

(*
let test_board_ =
    [
      Some( 1); Some( 2); Some(-1); Some(11);   Some(-1); Some(-1); Some(-1); Some(16);   Some(-1); Some(-1); Some(14); Some(-1);   Some(-1); Some(-1); Some( 4); Some(-1);
      Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(11); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some( 1); Some(-1);
      Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some( 3); Some( 5); Some(-1);
      Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some( 7);   Some(-1); Some(-1); Some(16); Some(-1);

      Some(-1); Some( 8); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some( 1);
      Some( 2); Some( 5); Some(13); Some(10);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some( 1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some( 4);
      Some(-1); Some( 3); Some(-1); Some(-1);   Some( 4); Some( 6); Some(16); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some( 7);
      Some(-1); Some( 9); Some(-1); Some(12);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some( 2); Some(-1);   Some(-1); Some(-1); Some(-1); Some( 8);

      Some(-1); Some(-1); Some( 2); Some(-1);   Some(-1); Some( 5); Some(-1); Some(-1);   Some(-1); Some(-1); Some( 3); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1); (*6 9 10 12 13 15*)
      Some(-1); Some(-1); Some( 8); Some(-1);   Some(-1); Some(-1); Some( 3); Some(-1);   Some(-1); Some( 5); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);
      Some(-1); Some(-1); Some( 6); Some(-1);   Some( 2); Some(-1); Some(-1); Some(13);   Some(12); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);
      Some( 5); Some(-1); Some(-1); Some(-1);   Some( 1); Some(14); Some(12); Some(-1);   Some( 1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);

      Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some( 2);
      Some( 3); Some( 2); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some( 7); Some(-1); Some(-1); Some(11);
      Some(-1); Some(-1); Some(-1); Some(-1);   Some( 3); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(-1);   Some(-1); Some(-1); Some(-1); Some(14);
      Some(12); Some(-1); Some(-1); Some( 7);   Some(-1); Some(-1); Some(-1); Some(-1);   Some( 8); Some( 9); Some(10); Some(11);   Some(-1); Some(-1); Some(-1); Some(16);
    ] ;;

let noneify b =
    List.map (function m ->
        match m with
            | Some(-1) -> None
            | Some(a) -> Some(a)
            | None -> None
    ) b;;

let test_board = noneify test_board_
*)

(* List.filter_map introduced in 4.08, but I'm using 4.07 T_T *)
let filter_map f l =
    let unwrap a = (
        match a with
            | Some(b) -> b
            | _ -> assert false
    )
    in List.map unwrap (
        List.filter ((!= ) None) (
            List.map f l
        )
    )
    ;;

(* Given a board, and a list of bools, return the "sub-board" (a row or column or block, normally, but can be an arbitrary pattern) *)
let subboard a_board pattern =
    let filt b p = match p with
        | true -> Some(b)
        | false -> None
    in
    filter_map (function a -> a) (List.map2 filt a_board pattern)
    ;;

(* Construct a list of length len where every item is 'value' *)
let const_list value len =
    let f _ = value
    in List.init len f
    ;;

let lfalse len = const_list false len;;
let ltrue len = const_list true len;;

(* Take a list l and repeat it n times *)
let rec repeat l n =
    match n with
        | 0 -> []
        | 1 -> l
        | _ -> List.append l (repeat l (n - 1))
    ;;

(* List of bools, true for each cell in row n, false otherwise *)
let row_pattern n =
    let low = w * n and high = w * (n + 1)
    in List.append (List.append (lfalse low) (ltrue w)) (lfalse ((w * w) - high))
    ;;

(* List of bools, true for each cell in column n, false otherwise *)
let col_pattern n =
    let before = n and after = ((w - n) - 1) in
    let one_row = List.append (lfalse before) (List.cons true (lfalse after))
    in repeat one_row w
    ;;

(* List of bools, true for each cell in block n, false otherwise *)
let block_pattern n =
    let x = (n mod rw) and y = (n / rw) in
    let on_row = List.append (lfalse (x*rw)) (List.append (ltrue rw) (lfalse (rw * (rw - (x+1)))))
    and off_row = lfalse w
    in List.append (repeat off_row (y*rw)) (List.append (repeat on_row rw) (repeat off_row (rw * (rw - (y+1)))))
    ;;

(* The domain that contains all possible cell solutions for a given board state
   this is simply a grid where each cell is a singleton set containing the
   input constraint, or is the domain of a cell (e.g. 1 to 9 for a 9x9 puzzle) *)
let domain board =
    let this_cell_domain i v =
        match v with
            | Some(a) -> IntSet.singleton a
            | None -> cell_domain
    in List.mapi this_cell_domain board
    ;;

(* Imperitive code! Oh my! (this function was hard to think about functionally) :P *)
let splice_domain domain new_vals pattern =
    let sz = List.length domain
    and result = ref []
    and v_index = ref 0 in
    for i = 0 to sz-1 do
        match List.nth pattern i with
            | true -> ((result := List.append (!result) [(List.nth new_vals (!v_index))]); (v_index := (!v_index) + 1))
            | false -> (result := List.append (!result) [(List.nth domain i)]);
    done;
    !result
    ;;

(* List of all pairs of elements (a, b) with a from l1 and b from l2 *)
let cross l1 l2 =
    let map1 i = List.map (function j -> (i, j)) l2
    in List.flatten (List.map map1 l1)
    ;;

(* Number of instances of item i in list l *)
let count_same i l =
    List.length (List.filter (IntSet.equal i) l)
    ;;

(* Given a list of sets of the same cardinality, returns a list of sets that appear in the given list exactly the same number
   of times as the cardinality of the input sets
   for example, given these input sets:
      [{1,2,3}, {4,5,6}, {7,8,9}, {1,2,3}, {1,2,3} {4,5,6}, {4,5,6}]
   this function returns the following list:
      [{1,2,3}, {4,5,6}]
   because the cardinality of the sets is 3, and the input list contains 3 occurrences of {1,2,3} and 3 occurrences of {4,5,6}
 *)
let reducable_groups sets =
    let n = IntSet.cardinal (List.hd sets) in
    filter_map (function s ->
        if (count_same s sets) == n then Some(s) else None
    ) sets
    ;;

let reduce_option_sets_for_size n sets =
    let size_is_n s = (IntSet.cardinal s) = n in
    let sets_of_size_n = List.filter size_is_n sets in
    let num_sets_of_size_n = List.length sets_of_size_n in
    let reduce_set s base = if IntSet.equal s base then s else IntSet.diff s base in
    if num_sets_of_size_n < n then sets else
        let subgroups = reducable_groups sets_of_size_n
        and reduce_one next_sets base_set = List.map (function s -> reduce_set s base_set) next_sets
        in List.fold_left reduce_one sets subgroups
    ;;

let reduce_domain_for_size_and_group n d g =
    let old_sets = subboard d g in
    let new_sets = reduce_option_sets_for_size n old_sets in
    splice_domain d new_sets g
    ;;

let reduce_domain_for_size n d =
    let patterns = [row_pattern; col_pattern; block_pattern]
    and range = List.init w (function i -> i)
    in let reduce domain (pattern, index) =
        reduce_domain_for_size_and_group n domain (pattern index)
    in List.fold_left reduce d (cross patterns range)
    ;;

let rec reduce_domain_all_sizes n d =
    match n with
        | 0 -> d (* base case is zero case, already solved by construction *)
        | _ -> (* all other cases solved from largest sets to smallest sets *)
            let d1 = reduce_domain_for_size n d
            in reduce_domain_all_sizes (n - 1) d1
    ;;

let reduce_domain d =
    pp "\n###########################\n\n";
    reduce_domain_all_sizes (w - 1) d
    ;;

(* find the board, in the given domain, containing all fully constrained cells *)
let fully_constrained_cells domain =
    let singleton s = if IntSet.cardinal s = 1 then Some(IntSet.choose s) else None
    in List.map singleton domain
    ;;

let domains_equal a b =
    List.fold_left (&&) true (List.map2 (IntSet.equal) a b)

(* Apply the constraint solver repeatedly until board is stable *)
let rec solved_domain d =
    let d1 = reduce_domain d in
    let stable = domains_equal d d1
    in print_domain d rw;
    if stable then d1 else solved_domain d1
    ;;

(* Solve an input board *)
let solved b =
    let d = domain b
    in fully_constrained_cells (solved_domain d)
    ;;

Printf.printf "Initial board state...\n" ;;
print_board test_board;;

let solved_board = solved test_board ;;

Printf.printf "After solving:\n" ;;
print_board solved_board rw;;
