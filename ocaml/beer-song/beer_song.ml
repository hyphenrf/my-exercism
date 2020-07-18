let rec recite from until =
    let num = function
      | -1 -> "99 bottles of beer"
      | 0  -> "no more bottles of beer"
      | 1  -> "1 bottle of beer"
      | n  -> (string_of_int n) ^ " bottles of beer"
    in
    let res = function
      | 0 -> "Go to the store and buy some more, "
      | 1 -> "Take it down and pass it around, "
      | _ -> "Take one down and pass it around, "
    in
    let sing i = String.capitalize_ascii (num i) 
                 ^ " on the wall, " ^ num i ^ ".\n"
                 ^ res i ^ num (i-1) ^ " on the wall."
    in
    let rec recite' ?(s="") from until =
        match from, until with
        | _, 1 | 0, _ -> s ^ sing from
        | _           -> recite' ~s:(s ^ sing from ^ "\n\n") (from-1) (until-1)
    in
    recite' from until
