let transform l =
  let rec rtrans = function
    | [] -> []
    | (s, cs) :: scss ->
      List.map (fun c -> (Char.lowercase_ascii c), s) cs
      :: rtrans scss
  in
    rtrans l
    |> List.concat
    |> List.sort (fun (a, _) (b, _) -> Char.compare a b)

