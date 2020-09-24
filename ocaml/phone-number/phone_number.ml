open Base

type token = Number  of char (*2-9*)
           | Punct   of char (*.-()\s*)
           | Invalid of badch
and
     badch = Alpha
           | Other

let token_of_char = function
    | '0'..'9' as x                           -> Number x
    | '+' | '(' | ')' | '.' | '-' | ' ' as x  -> Punct x
    | 'a'..'z' | 'A'..'Z'                     -> Invalid Alpha
    | _                                       -> Invalid Other

let char_of_token = function
    | Number x | Punct x -> x
    | Invalid _ -> 'X'

let (>>|) = Result.(>>|)
let (>>=) = Result.(>>=)

(**********************************)
let clean_punct =
    Array.filter ~f:(function Punct _ -> false | _ -> true)

let tokens_to_string arr =
    String.init (Array.length arr) ~f:(fun n -> char_of_token arr.(n))

let remove_cleaned_country_code =
    String.chop_prefix_if_exists ~prefix:"1"

(**********************************)
let find_invalids arr =
    match
    Array.find arr ~f:(function Invalid _ -> true | _ -> false)
    with
    | Some a -> begin 
        match a with
        | Invalid Alpha -> Error "letters not permitted"
        | Invalid Other -> Error "punctuations not permitted"
        | _ -> assert false
    end
    | None -> Ok arr

let verify_codes ?(start=0) arr =
    match arr.(start+0), arr.(start+3) with
    | Number '0', _ -> Error "area code cannot start with zero"
    | Number '1', _ -> Error "area code cannot start with one"
    | _, Number '0' -> Error "exchange code cannot start with zero"
    | _, Number '1' -> Error "exchange code cannot start with one"
    | _ -> Ok arr

let verify_values arr =
    match Array.length arr with
    | n when n > 11 -> Error "more than 11 digits"
    | n when n < 10 -> Error "incorrect number of digits"
    | 10 -> verify_codes arr
    | 11 -> begin match arr.(0) with 
                  | Number '1' -> verify_codes ~start:1 arr
                  | _ -> Error "11 digits must start with 1"
    end 
    | _ -> assert false

(**********************************)
let number s =
    s |> String.to_array
      |> Array.map ~f:token_of_char
      |> clean_punct
      |> find_invalids
     >>= verify_values
     >>| tokens_to_string
     >>| remove_cleaned_country_code
