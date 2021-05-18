(* We need a type to represent a set of integers *)
module Int = struct
    type t = int
    let compare = compare
end
module IntSet = Set.Make(Int);;
