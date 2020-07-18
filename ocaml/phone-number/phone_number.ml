open Base

type invalid = IP | IN

type num_t = Zero
           | One
           | Plus
           | N of char (*2-9*)
           | P of char (*.-()\s*)
           | Invalid of invalid

let num_of_char = function
    | '0' -> Zero
    | '1' -> One
    | '+' -> Plus

    | '2'..'9'                    as x -> N x
    | '(' | ')' | '.' | '-' | ' ' as x -> P x

    | 'a'..'z' | 'A'..'Z' -> Invalid IN
    | _                   -> Invalid IP

let char_of_num = function
    | Zero -> '0'
    | One -> '1'
    | N x -> x

let parse_num_array ?(start=0) arr =
    let area_code = Array.get arr (start+0) in
    let exch_code = Array.get arr (start+3) in
    match area_code, exch_code with
    | Zero, _ -> Error "area code cannot start with zero"
    | One,  _ -> Error "area code cannot start with one"
    | _, Zero -> Error "exchange code cannot start with zero"
    | _, One  -> Error "exchange code cannot start with one"
    | _ -> Ok (String.init 10 
                 ~f:(fun n -> char_of_num @@ Array.get arr (n+start)))


let verify_num_array a =
    match 
        Array.find a ~f:(function Invalid _ -> true | _ -> false)
    with
    | Some Invalid n -> 
       begin match n with
             | IP -> Error "punctuations not permitted"
             | IN -> Error "letters not permitted"
       end
    | None -> 
       begin
         let a = Array.filter ~f:(function Plus | P _ -> false | _ -> true) a in
         let len = Array.length a in
         match len with
         | a when a < 10 -> Error "incorrect number of digits"
         | a when a > 11 -> Error "more than 11 digits"
         | 11 -> begin Array.get a 0 |> 
                       function 
                       | N _ | Zero -> Error "11 digits must start with 1"
                       | One -> parse_num_array a ~start:1
                 end
         | 10 -> parse_num_array a
       end


let number s =
    s |> String.to_array
      |> Array.map ~f:num_of_char
      |> verify_num_array
