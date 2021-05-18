module Int = struct
    type t = int
    let compare = compare
end
module IntSet = Set.Make(Int);;
(*
type cell = int option ;;
type board = cell list ;;
type domain_board = IntSet.t list ;;
*)
