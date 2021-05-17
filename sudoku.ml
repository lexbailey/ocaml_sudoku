let rw = 3 ;; (* root width. 2 means you get a 4*4 board, 3 means 9*9 *)

let w = rw * rw ;; (* width *)
let n_cells = w * w ;; (* total number of cells on the board *)

module Int = struct
    type t = int
    let compare = compare
end
module IntSet = Set.Make(Int);;

let pp = Printf.printf ;;

type cell = int option ;;
type board = cell list ;;
type domain_board = IntSet.t list ;;

(* Set of all numbers that can be placed in a cell *)
let cell_domain = IntSet.of_list (List.init w (fun x -> x+1)) ;;

let max_cell_width = Float.to_int (Float.ceil (Float.log10 (Float.of_int w)));;

(*
let test_board: board =
    [ None; None; None; Some(1)
    ; Some(1); Some(2); None; None
    ; None; None; None; None
    ; None; None; None; None
    ] ;;
*)


let test_board: board =
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
let test_board_: board =
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

let test_board: board = noneify test_board_
*)

let rec take n l = (* yikes why is this not a builtin *)
    match n with
        | 0 -> []
        | _ -> if ((List.length l) > 0) then List.cons (List.hd l) (take (n-1) (List.tl l)) else []
    ;;

let rec drop n l =
    match n with
        | 0 -> l
        | _ -> if (List.length l) > 0 then drop (n-1) (List.tl l) else []
    ;;

let rec sublists l max_len =
    let start = (take max_len l)
    and _end = (drop max_len l)
    in List.append [start;] (if (List.length _end = 0) then [] else (sublists _end max_len))
    ;;

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

let n_solved b =
    List.length (List.filter ((!=) None) b)
    ;;

(* Given a board, and a list of bools, return the "sub-board" (a row or column or block, normally, but can be an arbitrary pattern) *)
let subboard a_board pattern =
    let filt b p = match p with
        | true -> Some(b)
        | false -> None
    in
    filter_map (function a -> a) (List.map2 filt a_board pattern)
    ;;

let str c = match c with
    | None -> "?"
    | Some(n) -> string_of_int n
    ;;

let rec repeat_str s c n =
    match n with
        | 1 -> s
        | _ -> repeat_str (s ^ c) c (n-1)
    ;;

let repeat_char c n =
    repeat_str c c n
    ;;

let box_top n = "┌" ^ (repeat_char "─" n) ^ "┐" ;;
let box_bottom n = "└" ^ (repeat_char "─" n) ^ "┘" ;;
let box_section s = "│" ^ s ^ "│" ;;

let grid_line n cols left mid right =
    let bar = (repeat_char "─" n) in
    left ^ (String.concat mid (List.init cols (function _ -> bar))) ^ right
    ;;

let grid_top n cols = grid_line n cols "┌" "┬" "┐" ;;
let grid_mid n cols = grid_line n cols "├" "┼" "┤" ;;
let grid_bot n cols = grid_line n cols "└" "┴" "┘" ;;

let print_board b =
    let lines = sublists b w in
    List.iteri (fun i l ->
        let llen = (List.length l)
        and bwidth = max_cell_width + 2
        in
        Printf.printf "%s\n%s\n%s"
            (if (i = 0) then (grid_top bwidth llen) else (grid_mid bwidth llen))
            (box_section (String.concat "│" (List.map (function a -> ((match a with | Some(n) -> Printf.sprintf " %*d " max_cell_width n | None -> (repeat_char " " bwidth)))) l)))
            (if ((i+1) = (List.length lines)) then (grid_bot bwidth llen) else "")
            (*(grid_bot 1 (List.length l))*)
    ) lines ;
    Printf.printf "\n"
    ;;

let const_list value len =
    let f _ = value
    in List.init len f
    ;;

let lfalse len = const_list false len;;
let ltrue len = const_list true len;;

let rec repeat l n =
    match n with
        | 0 -> []
        | 1 -> l
        | _ -> List.append l (repeat l (n - 1))
    ;;

(* Extract a row of cells from a board *)
let row_pattern n =
    let low = w * n and high = w * (n + 1)
    in List.append (List.append (lfalse low) (ltrue w)) (lfalse ((w * w) - high))
    ;;

(* Extract a column of cells from a board *)
let col_pattern n =
    let before = n and after = ((w - n) - 1) in
    let one_row = List.append (lfalse before) (List.cons true (lfalse after))
    in repeat one_row w
    ;;

(* Extract a block from a board *)
let block_pattern n =
    let x = (n mod rw) and y = (n / rw) in
    let on_row = List.append (lfalse (x*rw)) (List.append (ltrue rw) (lfalse (rw * (rw - (x+1)))))
    and off_row = lfalse w
    in List.append (repeat off_row (y*rw)) (List.append (repeat on_row rw) (repeat off_row (rw * (rw - (y+1)))))
    ;;

let row n b = subboard b (row_pattern n);;
let col n b = subboard b (col_pattern n);;
let block n b = subboard b (block_pattern n);;

let print_set s =
    Printf.printf "[";
    List.iter (function a -> Printf.printf "%d," a) (IntSet.elements s) ;
    Printf.printf "]\n";;

let print_list_of_sets l =
    List.iter print_set l;;

let print_list_of_bool l =
    List.iter (pp "%B, ") l ; pp "\n" ;;

let print_list_of_int l =
    List.iter (pp "%d, ") l; pp "\n" ;;

Printexc.record_backtrace true;;
Printf.printf "Initial board state...\n" ;;
print_board test_board;;

(* Compute the domain that contains all possible cell solutions for a given board state *)
let domain board =
    let this_cell_domain i v =
        match v with
            | Some(a) -> IntSet.singleton a
            | None -> cell_domain
    in List.mapi this_cell_domain board
    ;;

let nth_of_each n l =
    List.map (function sl -> match List.nth_opt sl n with | Some(a) -> a | None -> "") l
    ;;

(* s is a list of lists of strings *)
let boxes s bw bh =
    let top = String.concat " " (List.map (function _ -> box_top bw) s)
    and bot = String.concat " " (List.map (function _ -> box_bottom bw) s)
    and range = List.init bh (function i -> i) in
        let mids = List.map (function ss -> String.concat " " (List.map box_section ss)) (List.map (function n -> nth_of_each n s) range)
        in let mid = String.concat "\n" mids
        in String.concat "\n" [top; mid; bot; ""]
    ;;

let padding = repeat_char " " ;;

let normalise_widths ss n max_width =
    List.map (function s ->
        let l = List.map
            (function text ->
                let cur_len = String.length text
                in if cur_len > max_width then assert false else text ^ (padding (max_width - cur_len))
            )
        s
        in let n_in_list = List.length l
        in List.append l (List.init (n - n_in_list) (function _ -> padding max_width))
    ) ss
    ;;

let sets_to_lines ss el_width els_per_line =
    List.map (function s ->
        List.map (function l -> String.concat " " l) (
            sublists (
                List.map (function i -> Printf.sprintf "%*d" el_width i) (IntSet.elements s)
            ) els_per_line
        )
    ) ss
    ;;

let box_grid b width height el_width els_per_line max_el_lines =
    let lines = sublists b width
    and bw = els_per_line * (el_width + 1)
    in
    String.concat "" (
        List.map (function l ->
            boxes (normalise_widths (sets_to_lines l el_width els_per_line) max_el_lines bw )
            bw
            max_el_lines
        ) lines
    )
    ;;

let print_domain d = Printf.printf "%s" (box_grid d w w rw rw rw) ;;

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

let count_same i l =
    List.length (List.filter (IntSet.equal i) l)
    ;;

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

let cross l1 l2 =
    let map1 i = List.map (function j -> (i, j)) l2
    in List.flatten (List.map map1 l1)
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

(* Solve simple constraints until board is stable *)
let rec solved_domain d =
    let d1 = reduce_domain d
    in let stable = domains_equal d d1
    in
    print_domain d;
    if stable then d1 else solved_domain d1
    ;;

let solved b =
    let d = domain b
    in fully_constrained_cells (solved_domain d)

let solved_board = solved test_board ;;

Printf.printf "After solving:\n" ;;
print_board solved_board;;
