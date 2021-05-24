(*

A sudoku solver written in ocaml

Designed to be simple in the way that it works, applying a fairly simple algorithm
that can solve arbitrary sized input puzzles as far as possible given the existing
input constraints.

Some terms we will define before describing the algoithm:

 - board: A square grid of side length N*N for a non-zero natural number N.
          each cell in the grid is either None or Some(a) where a is an integer
          in the range 1 to N*N.
          Note that this definition means that the side length of the board is always
          a square number. This is why the board can be split into N*N square boxes
          of side length N (and thus each box contains n*n cells)

 - box: One square sub-board of a board with side length sqrt(M) where M is the side
        length of the board. boxes do not overlap, there is exactly M boxes per board

 - cell domain: the set of possible values that a cell can have.
                For a size 3 puzzle (board side length is 9) the fully unconstrained
                cell domain is {1,2,3,4,5,6,7,8,9}.
                A cell's final value is known when its domain is a singleton set.
                If a cell's domain is ever reduced to the empty set then the board is
                over-constrained

 - board domain: a square grid of cell domains, otherwise matching the form of a
                 regular board

 - constraint group: For a board of side length M, a constraint group is a set of M cells
                     where all cells in the group must contain unique values.
                     Since there will only be M possible values on a board of side length M
                     this means that each value appears exactly once in the constraint group.
                     There are three different types of constraint group on every sudoku
                     board. These are rows, columns, and boxes.
                     That is, a constraint group contains one row, one column, or one box
                     from a board or a board domain.

A puzzle is considered solved when it is not possible to further reduce any of the cell
domains without adding additional input constraints.

The algorithm:

   Starting with an input puzzle board of side length M=N*N with some empty cells and some fixed-value cells:

   1) Construct a board domain where each cell domain is:
      a singleton set if the input cell has a value, or
      the fully unconstrained cell domain if the input cell is empty

   2) For each value of I in the range M-1 down to 1:
          For each type T of constraint group:
              For each constraint groups C of type T: (of which there are M)
                  For each cell domain S of size I in C that appears I times in C:
                      For each cell domain B in C that is not equal to S:
                          Let B' = B - A
                          replace B with B' in C

   3) If any B' was not equal to B in step 2 then repeat step 2 and reevaluate step 3 before
      continuing to step 4

   4) Construct the output board from the new [reduced] board domain as follows:
      - Each cell domain that is a singleton set becomes a cell containing the value in the singleton
      - Each cell domain with cardinality >= 2 becomes an empty cell


*)
Printexc.record_backtrace true;;
open Types;;
open Printutils;;

let nargs = (Array.length Sys.argv) - 1;;
if nargs != 1 then (Printf.printf "Please specify an input filename.\n" ; exit 1);;

let filename = Sys.argv.(1) ;;

(* rw is root width. 2 means you get a 4*4 board, 3 means 9*9
   input_board is a list of int options *)
let rw, input_board = Puzzle_loader.puzzle_from_file filename;;

let w = rw * rw ;; (* width *)
let n_cells = w * w ;; (* total number of cells on the board *)

let pp = Printf.printf ;;

(* Set of all numbers that can be placed in a cell *)
let cell_domain = IntSet.of_list (List.init w (fun x -> x+1)) ;;

(* Check for invalid cells *)
List.iter (fun cell ->
    match cell with
        | Some(c) -> (if not (IntSet.exists ((==) c) cell_domain) then (Printf.printf "Board contains cells with invalid values. %d is not allowed on a board of side length %d\n" c w ; exit 1))
        | None -> ()
) input_board



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
    in filter_map (function a -> a) (List.map2 filt a_board pattern)
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
    let before = n and after = ((w - n) - 1)
    in let one_row = List.append (lfalse before) (List.cons true (lfalse after))
    in repeat one_row w
    ;;

(* List of bools, true for each cell in block n, false otherwise *)
let block_pattern n =
    let x = (n mod rw) and y = (n / rw)
    in let on_row = List.append (lfalse (x*rw)) (List.append (ltrue rw) (lfalse (rw * (rw - (x+1)))))
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

(* Imperitive code! Oh my! (this function was hard to think about functionally) :P
   Take a board domain (a list), a list of values to splice in as replacements, and a pattern of
   bools of same length as the board domain, and splice the replacements in where the bool list
   contains true
   For example, where a, b, c, d, e, f are all cell domains:
      splice_domain [a; b; c; d] [e; f] [false, true, true, false] = [a; e; c; f]
 *)
let splice_domain domain new_vals pattern =
    let sz = List.length domain
    and result = ref []
    and v_index = ref 0
    in for i = 0 to sz-1 do
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
    let n = IntSet.cardinal (List.hd sets)
    in filter_map (function s ->
        if (count_same s sets) == n then Some(s) else None
    ) sets
    ;;

(* Take a list of domains of cells in a constraint group, reduce the domains of all other cells in the
   group for any suitable subgroups of size n *)
let reduce_option_sets_for_size n sets =
    let size_is_n s = (IntSet.cardinal s) = n
    in let sets_of_size_n = List.filter size_is_n sets
    in let num_sets_of_size_n = List.length sets_of_size_n
    in let reduce_set s base = if IntSet.equal s base then s else IntSet.diff s base
    in if num_sets_of_size_n < n then sets else  (* This check not strictly required, but skips some computation *)
        let subgroups = reducable_groups sets_of_size_n
        and reduce_one next_sets base_set = List.map (function s -> reduce_set s base_set) next_sets
        in List.fold_left reduce_one sets subgroups
    ;;

(* apply domain reduction for each size from largest to smallest *)
let reduce_domain d =
    let reduce_domain_for_size_and_group n d g =
        let old_sets = subboard d g
        in let new_sets = reduce_option_sets_for_size n old_sets
        in splice_domain d new_sets g
    in let reduce_domain_for_size n d =
        let patterns = [row_pattern; col_pattern; block_pattern] (* Three different constraint group types*)
        and range = List.init w (function i -> i) (* For each set type, there's 'w' different groups *)
        in let reduce domain (pattern, index) =
            reduce_domain_for_size_and_group n domain (pattern index)
        in List.fold_left reduce d (cross patterns range)
    in let rec reduce_domain_inner n d =
        match n with
            | 0 -> d (* base case is zero case, already solved by construction *)
            | _ -> (* all other cases solved from largest sets to smallest sets *)
                let d1 = reduce_domain_for_size n d
                in reduce_domain_inner (n - 1) d1
    in pp "\n###########################\n\n";
    reduce_domain_inner (w - 1) d
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
    let d1 = reduce_domain d
    in let stable = domains_equal d d1
    in print_domain d rw;
    if stable then d1 else solved_domain d1
    ;;

(* Solve an input board *)
let solved b =
    let d = domain b
    in let final_domain = solved_domain d
    in (fully_constrained_cells final_domain, final_domain)
    ;;

(* Just ocaml 4.07 things ;P *)
let is_some a = match a with | Some(_) -> true | None -> false ;; (* Option.is_some introduced in 4.08 *)

let is_fully_constrained b =
    (List.length (List.filter (is_some) b)) == (List.length b)
    ;;

Printf.printf "Initial board state...\n" ;;
print_board input_board rw;;

let find_dups l s =
    let sl = List.of_seq (IntSet.to_seq s) in
    filter_map (fun i ->
        if List.length (List.find_all ((==) i) l) > 1 then Some(i) else None
    ) sl
    ;;

(* Check for overconstrained start conditions *)
let check_overconstrained board =
    let patterns = [("Row",row_pattern); ("Column", col_pattern); ("Block", block_pattern)] in
    let range = List.init w (function i -> i) in
    let check ((name, pattern), index) =
        let group = subboard board (pattern index) in
        let filtered = filter_map (fun a -> a) group in
        let set = IntSet.of_seq (List.to_seq filtered) in
        if (List.length filtered) > (IntSet.cardinal set) then
            let duplicates = find_dups filtered set in
            Printf.printf "Input puzzle is overconstrained.\n%s %d contains multiple instance of the value %d\n" name (index + 1) (List.hd duplicates); exit 1
    in List.iter check (cross patterns range)
    ;;

let is_unsolvable domain =
    let emptys = List.filter (IntSet.is_empty) domain in
        (List.length emptys) > 0
    ;;

check_overconstrained input_board ;;

let (solved_board, final_domain)  = solved input_board ;;

if is_unsolvable final_domain then
    Printf.printf "The input board has no solutions. Some cells were found to have empty domains.\n"
else (
    if not (is_fully_constrained solved_board) then
        Printf.printf "The input board has multiple solutions.\n"
    else
        Printf.printf "The input board has exactly one solution.\n";
    print_board solved_board rw
)
;;
