module String = Base.String
module Char   = Base.Char
module Result = Base.Result

(*********************************************)
let string_partition f = String.fold
    ~init:("", "") 
    ~f:(fun (l, r) c -> 
        if f c 
        then l ^ Base.String.of_char c, r 
        else l, r ^ Base.String.of_char c)

let invalid_punct = function
    | '+' | '-' | '.' | ' ' | '(' | ')' -> false
    | _ -> true

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )

let ( ?>! ) cond err okv = if cond then Error err else Ok okv
let ( .?[] ) = String.unsafe_get

(*********************************************)
let number s =
    let nums, chars = string_partition Char.is_digit s in
    let numlen      = String.length nums in
    
    Ok nums
    >>= ?>! (String.exists chars ~f:Char.is_alpha) "letters not permitted"
    >>= ?>! (String.exists chars ~f:invalid_punct) "punctuations not permitted"
    >>= ?>! (numlen = 11 && nums.?[0] <> '1')      "11 digits must start with 1"
    >>= ?>! (numlen > 11)                          "more than 11 digits"
    >>= ?>! (numlen < 10)                          "incorrect number of digits"
    >>= ?>! (nums.?[numlen-10] = '0')              "area code cannot start with zero"
    >>= ?>! (nums.?[numlen-10] = '1')              "area code cannot start with one"
    >>= ?>! (nums.?[numlen-7]  = '0')              "exchange code cannot start with zero"
    >>= ?>! (nums.?[numlen-7]  = '1')              "exchange code cannot start with one"
    
    >>| String.chop_prefix_if_exists ~prefix:"1"
