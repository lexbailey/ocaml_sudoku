open Types;;

(* Util functions for printing boards and board domains in prety ways *)

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
    let bar = (repeat_char "─" n)
    in left ^ (String.concat mid (List.init cols (function _ -> bar))) ^ right
    ;;

let grid_top n cols = grid_line n cols "┌" "┬" "┐" ;;
let grid_mid n cols = grid_line n cols "├" "┼" "┤" ;;
let grid_bot n cols = grid_line n cols "└" "┴" "┘" ;;

let rec sublists l max_len =
    let start = (take max_len l)
    and _end = (drop max_len l)
    in List.append [start;] (if (List.length _end = 0) then [] else (sublists _end max_len))
    ;;

let max_cell_width w = Float.to_int (Float.ceil (Float.log10 (Float.of_int w)));;

let print_board b rw =
    let w = rw * rw
    in let lines = sublists b w
    in List.iteri (fun i l ->
        let llen = (List.length l)
        and bwidth = (max_cell_width w) + 2
        in Printf.printf "%s\n%s\n%s"
            (if (i = 0) then (grid_top bwidth llen) else (grid_mid bwidth llen))
            (box_section (String.concat "│" (List.map (function a -> ((match a with | Some(n) -> Printf.sprintf " %*d " (max_cell_width w) n | None -> (repeat_char " " bwidth)))) l)))
            (if ((i+1) = (List.length lines)) then (grid_bot bwidth llen) else "")
    ) lines ;
    Printf.printf "\n"
    ;;

let nth_of_each n l =
    List.map (function sl -> match List.nth_opt sl n with | Some(a) -> a | None -> "") l
    ;;

(* s is a list of lists of strings *)
let boxes s bw bh =
    let top = String.concat " " (List.map (function _ -> box_top bw) s)
    and bot = String.concat " " (List.map (function _ -> box_bottom bw) s)
    and range = List.init bh (function i -> i)
    in let mids = List.map (function ss -> String.concat " " (List.map box_section ss)) (List.map (function n -> nth_of_each n s) range)
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
    in String.concat "" (
        List.map (function l ->
            boxes (normalise_widths (sets_to_lines l el_width els_per_line) max_el_lines bw )
            bw
            max_el_lines
        ) lines
    )
    ;;

let print_domain d rw = let w = rw * rw in Printf.printf "%s" (box_grid d w w rw rw rw) ;;
