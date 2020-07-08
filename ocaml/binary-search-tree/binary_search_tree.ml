(* open Base *)

type bst =
   | Leaf
   | Node of int * bst * bst


let (++) = List.append


let empty = Leaf

let value tree = match tree with
    | Leaf           -> Error "Empty tree."
    | Node (k, _, _) -> Ok k

let left tree = match tree with
    | Leaf           -> Error "Empty tree."
    | Node (_, l, _) -> Ok l

let right tree = match tree with
    | Leaf           -> Error "Empty tree."
    | Node (_, _, r) -> Ok r

let rec insert i tree = match tree with
    | Leaf           -> Node (i, Leaf, Leaf)
    | Node (k, l, r) -> if i <= k 
            then Node (k, insert i l, r)
            else Node (k, l, insert i r)

let rec to_list tree = match tree with
    | Leaf           -> []
    | Node (k, l, r) -> (to_list l) ++ (k::to_list r)

