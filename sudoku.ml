let rw = 2 ;; (* root width. 2 means you get a 4*4 board, 3 means 9*9 *)

let w = rw * rw ;; (* width *)
let n_cells = w * w ;; (* total number of cells on the board *)

module Int = struct
    type t = int
    let compare = compare
end
module IntSet = Set.Make(Int);;

type cell = int option ;;
type board = cell list ;;
type domain_board = IntSet.t list ;;

(* Set of all numbers that can be placed in a cell *)
let cell_domain = IntSet.of_list (List.init w (fun x -> x+1))

let test_board: board =
    [ None; None; None; Some(1)
    ; Some(1); Some(2); None; None
    ; None; None; None; None
    ; None; None; None; None
    ] ;;

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

(*
let b_to_s b = match b with | true -> "True" | false -> "False" ;;
let print_list b =
    let pr c =
        Printf.printf "%s " (b_to_s c)
    in List.iter pr b ;
    Printf.printf "\n"
    ;;
*)

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

(*
let print_board b =
    let pr1 i c =
        Printf.printf "%s%s" (str c) (if ((i+1) mod w) == 0 then "\n" else " ")
    in List.iteri pr1 b
    ;;
*)

let print_board b =
    let lines = sublists b w in
    List.iteri (fun i l ->
        let llen = (List.length l)
        and bwidth = 3
        in
        Printf.printf "%s\n%s\n%s"
            (if (i = 0) then (grid_top bwidth llen) else (grid_mid bwidth llen))
            (box_section (String.concat "│" (List.map (function a -> ((match a with | Some(n) -> Printf.sprintf " %d " n | None -> " ? "))) l)))
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

(* get Row, Column, and Block ids from cell id *)
let rcb i =
    let row = i / w
    and col = (i mod w)
    and block = (rw*(i/rw/rw/rw)) + ((i/rw) mod rw)
    in (row, col, block)
    ;;

let test_row = row 0 test_board;;
let test_col = col 3 test_board;;

Printexc.record_backtrace true;;
Printf.printf "Initial board state...\n" ;;
print_board test_board;;

(* Compute the domain that contains all possible cell solutions for a given board state *)
let domain board =
    let this_cell_domain i v =
        match v with
            | Some(a) -> IntSet.singleton a
            | None ->
                let (r, c, b) = rcb i in
                let rs = row r board
                and cs = col c board
                and bs = block b board
                in IntSet.diff cell_domain (IntSet.of_list (filter_map (function a -> a) (List.concat [rs; cs; bs])))
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

let reduce_option_sets_for_size n sets =
    let size_is_n s = (IntSet.cardinal s) = n in
    let num_sets_of_size_n = List.length (List.filter size_is_n sets) in
    if num_sets_of_size_n != n then sets else
        let base_set = List.find size_is_n sets in
        List.map (function s -> if (IntSet.cardinal s) > n then IntSet.diff s base_set else s) sets
    ;;

let reduce_domain_for_size_and_group n d g =
    print_domain d;
    let old_sets = subboard d g in
    let new_sets = reduce_option_sets_for_size n old_sets in
    print_domain (splice_domain d new_sets g);
    splice_domain d new_sets g
    ;;

let reduce_domain_for_size n d =
    (* d is a domain (a list of sets of allowed valus) and we need to return another domain *)
    (* this is the case for n, we need to operate on every group of size n in every constraint set
    in the domain where the cardinality of each set is n and the sets are all equal to each other *)
    (*let patterns = [row_pattern; col_pattern; block_pattern]*)
    let patterns = [col_pattern; row_pattern]
    and reduce domain pattern = reduce_domain_for_size_and_group n d (pattern 0)
    in List.fold_left reduce d patterns
    ;;

let rec reduce_domain_all_sizes n d =
    match n with
        | 0 -> d (* base case is zero case, already solved by construction *)
        | _ -> (* all other cases solved from largest sets to smallest sets *)
            let d1 = reduce_domain_for_size n d
            in reduce_domain_all_sizes (n - 1) d1
    ;;

let reduce_domain d =
    reduce_domain_all_sizes (w - 1) d
    ;;

(* find the board, in the given domain, containing all fully constrained cells *)
let fully_constrained_cells domain =
    let singleton s = if IntSet.cardinal s = 1 then Some(IntSet.choose s) else None
    in List.map singleton domain
    ;;

(* Solve simple constraints until board is stable *)
let rec solved b =
    let cur_n = n_solved b
    and dom = domain b
    in let next = fully_constrained_cells dom
    in let next_n = n_solved next
    in if (cur_n == next_n) then next else solved next
    ;;

let test_domain = domain test_board ;;
let test_domain2 = reduce_domain (domain test_board) ;;
let test_step = solved test_board ;;

let print_set s =
    Printf.printf "[";
    List.iter (function a -> Printf.printf "%d," a) (IntSet.elements s) ;
    Printf.printf "]\n";;

(*let largest sets =
    let max a b = if a > b then a else b
    in List.fold_left max 0 (List.map IntSet.cardinal sets)*)


let nth_of_each n l =
    List.map (function sl -> match List.nth_opt sl n with | Some(a) -> a | None -> "") l
    ;;

(* s is a list of lists of strings *)
let boxes s bw bh =
    let l = List.length s in
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

Printf.printf "The board state above describes a subset of the following domain...\n" ;;
print_domain test_domain ;;
print_domain test_domain2 ;;

Printf.printf "After solving simple constraints repeatedly until stability is reached:\n" ;;
print_board test_step;;
