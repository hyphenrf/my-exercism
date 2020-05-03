type nucleotide = A | C | G | T

(* Single-pass function *)
let hamming_distance l r =
  match l, r with
  | [], [] -> Ok 0
  | [], _  -> Error "left strand must not be empty"
  | _ , [] -> Error "right strand must not be empty"
  | _ , _  -> let ne a b = if a <> b then 1 else 0 in
    let rec count ?(n=0) = function
      | [], [] -> n
      | x::xs, y::ys -> count ~n:(n + (ne x y)) (xs, ys)
      | _ -> raise (Failure "left and right strands must be of equal length")
    in try
      Ok (count (l, r))
    with Failure msg ->
      Error msg

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
