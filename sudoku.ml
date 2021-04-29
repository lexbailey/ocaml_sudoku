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
        | 0 -> s
        | _ -> repeat_str (s ^ c) c (n-1)
    ;;

let repeat_char c n =
    repeat_str c c n
    ;;

let box_top n = "┌" ^ (repeat_char "─" n) ^ "┐" ;;
let box_bottom n = "└" ^ (repeat_char "─" n) ^ "┘" ;;
let box_section s = "│" ^ s ^ "│" ;;

let print_board b =
    let pr1 i c =
        Printf.printf "%s%s" (str c) (if ((i+1) mod w) == 0 then "\n" else " ")
    in List.iteri pr1 b
    ;;

let const_list value len =
    let f _ = value
    in List.init len f
    ;;

let lfalse len = const_list false len;;
let ltrue len = const_list true len;;

(* Extract a row of cells from a board *)
let row n b =
    let low = w * n and high = w * (n + 1)
    in let pattern = List.append (List.append (lfalse low) (ltrue w)) (lfalse ((w * w) - high))
    in subboard b pattern
    ;;

let rec repeat l n =
    match n with
        | 0 -> []
        | 1 -> l
        | _ -> List.append l (repeat l (n - 1))
    ;;

(* Extract a row of columns from a board *)
let col n b =
    let before = n and after = ((w - n) - 1) in
    let one_row = List.append (lfalse before) (List.cons true (lfalse after)) in
    let pattern = repeat one_row w
    in subboard b pattern
    ;;

(* Extract a block from a board *)
let block n b =
    let x = (n mod rw) and y = (n / rw) in
    let on_row = List.append (lfalse (x*rw)) (List.append (ltrue rw) (lfalse (rw * (rw - (x+1)))))
    and off_row = lfalse w
    in let pattern = List.append (repeat off_row (y*rw)) (List.append (repeat on_row rw) (repeat off_row (rw * (rw - (y+1)))))
    in subboard b pattern
    ;;

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
Printf.printf "Board...\n" ;;
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

(* find the board, in the given domain, containing all fully constrained cells *)
let fully_constrained_cells domain =
    let singleton s = if IntSet.cardinal s = 1 then Some(IntSet.choose s) else None
    in List.map singleton domain
    ;;

(* Solve simple constraints until board is stable *)
let rec resolve_simple b =
    let cur_n = n_solved b
    and dom = domain b
    in let next = fully_constrained_cells dom
    in let next_n = n_solved next
    in if (cur_n == next_n) then next else resolve_simple next
    ;;

let test_domain = domain test_board ;;
(*let test_step = fully_constrained_cells test_domain ;;*)
let test_step = resolve_simple test_board ;;

let print_set s =
    Printf.printf "[";
    List.iter (function a -> Printf.printf "%d," a) (IntSet.elements s) ;
    Printf.printf "]\n";;

let largest sets =
    let max a b = if a > b then a else b
    in List.fold_left max 0 (List.map IntSet.cardinal sets)


let nth_of_each n l =
    List.map (function sl -> match List.nth_opt sl n with | Some(a) -> a | None -> "") l
    ;;

(* s is a list of lists of strings *)
let boxes s bw bh =
    let l = List.length s in
        let top = String.concat " " (List.map (function _ -> box_top bw) s)
        and bot = String.concat " " (List.map (function _ -> box_bottom bw) s)
        and range = List.init bh (function i -> i) in
            (*List.iter (function a -> Printf.printf "[%d]" a) range;*)
            let mids = List.map (function ss -> String.concat " " (List.map box_section ss)) (List.map (function n -> nth_of_each n s) range)

 (*range.map (function i
                let 
                (function ns -> String.concat " " (List.map (function a -> box_section a) ns))*)
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
    String.concat "\n" (
        List.map (function l ->
            boxes (normalise_widths (sets_to_lines l el_width els_per_line) max_el_lines bw )
            bw
            max_el_lines
        ) lines
    )
    ;;

(*
Printf.printf "%s" (box_top 3) ;;
Printf.printf "%s" (box_bottom 3) ;;
Printf.printf "\n" ;;

Printf.printf "%s" (boxes [[" 0 1 2"; "123423"];["test"];["foo";"bar"]] 3 2 ) ;;
Printf.printf "%s" (boxes (normalise_widths [[" 0 1 2"; "123423"];["test"];["foo";"bar"]] 3 9) 9 2 ) ;;
Printf.printf "%s" (boxes (normalise_widths (sets_to_lines test_domain 1 3) 3 9) 9 2 ) ;;
Printf.printf "\n" ;;
Printf.printf "\n" ;;
*)
Printf.printf "%s" (box_grid test_domain w w rw rw rw) ;;

(*let set_square_string set max_size max_digits =
    let tostr a = Printf.sprintf "%*d" max_digits a
    in let strings = List.map tostr (IntSet.elements set)
    in *)

let print_domain d =
    Printf.printf "The largest set is of size %d\n" (largest d);
    List.map print_set d
    ;;

print_domain test_domain ;;

(*List.iter print_set test_domain
    ;;*)

Printf.printf "After solving simple constraints:\n" ;;
print_board test_step;;
