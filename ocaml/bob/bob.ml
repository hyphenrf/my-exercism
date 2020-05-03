let response_for s = match String.trim s with
  | "" -> "Fine. Be that way!"
  | cs -> 
    let cs_len     = String.length cs in
    let is_asking  = cs_len-1 |> String.get cs |> (=) '?' in
    let is_yelling =
      let is_lower c = 96 < Char.code c && Char.code c < 123 in
      let is_upper c = 64 < Char.code c && Char.code c < 91  in
      let rec cs_any ?(i=0) check =
        match i with
        | x when x = cs_len -> false
        | _ -> check cs.[i] || cs_any check ~i:(i+1)
      in 
      cs_any is_upper && not @@cs_any is_lower
    in
    match is_asking, is_yelling with
        | false,     false      -> "Whatever."
        | false,     true       -> "Whoa, chill out!"
        | true,      false      -> "Sure."
        | true,      true       -> "Calm down, I know what I'm doing!"

