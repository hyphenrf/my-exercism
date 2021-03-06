let is_lower = function 'a'..'z' -> true | _ -> false
let is_upper = function 'A'..'Z' -> true | _ -> false

let rec string_any ?(i=0) (fn: char -> bool) str =
    if i = String.length str then false
    else fn str.[i] || string_any ~i:(i+1) fn str

let response_for s = match String.trim s with
  | "" -> "Fine. Be that way!"
  | cs ->
    let cs_length  = String.length cs         in
    let is_asking  = cs.[cs_length - 1] = '?' in
    let is_yelling =
        string_any is_upper cs && not (string_any is_lower cs)
    in
    match is_asking, is_yelling with
        | false,     false      -> "Whatever."
        | false,     true       -> "Whoa, chill out!"
        | true,      false      -> "Sure."
        | true,      true       -> "Calm down, I know what I'm doing!"

