type nucleotide = A | C | G | T

let int_of_bool x = if x then 1 else 0

(* Single-pass function *)
let hamming_distance l r =
  match l, r with
  | [], [] -> Ok 0
  | [], _  -> Error "left strand must not be empty"
  | _ , [] -> Error "right strand must not be empty"
  | _ , _  -> 
    let rec count n = function
      | [],[] -> n
      | x::xs, y::ys -> count (n + (int_of_bool (x<>y))) (xs, ys)
      | _ -> -1
    in match count 0 (l, r) with
      | -1 -> Error "left and right strands must be of equal length"
      |  n -> Ok n

(* With composition -- OCaml is eager, so this is probably more expensive? *)
let hamming_composed l r = match l, r with
  | [], [] -> Ok 0
  | [], _  -> Error "left strand must not be empty"
  | _ , [] -> Error "right strand must not be empty"
  | _ , _  -> 
    try Ok 
      (  List.combine l r
      |> List.filter (fun (x, y) -> x <> y)
      |> List.length )
    with _ -> Error "left and right strands must be of equal length"
