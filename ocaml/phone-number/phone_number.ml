open Base
(* I feel like I should revisit this solution and try for something more elegant *)

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
    | _ -> raise (Invalid_argument "char_of_num")

let parse_num_array ?(start=0) arr =
    let area_code = arr.(start+0) in
    let exch_code = arr.(start+3) in
    match area_code, exch_code with
    | Zero, _ -> Error "area code cannot start with zero"
    | One,  _ -> Error "area code cannot start with one"
    | _, Zero -> Error "exchange code cannot start with zero"
    | _, One  -> Error "exchange code cannot start with one"
    | _ -> Ok (String.init 10 ~f:(fun n -> char_of_num arr.(n+start)))


let verify_num_array a =
    match 
        Array.find a ~f:(function Invalid _ -> true | _ -> false)
    with
    | Some n -> 
       begin match n with
             | Invalid IP -> Error "punctuations not permitted"
             | Invalid IN -> Error "letters not permitted"
             | _ -> failwith "unreachable"
       end
    | None -> 
       begin
         Array.filter a ~f:(function Plus | P _ -> false | _ -> true)
         |> (fun a -> match Array.length a with
         | 11 -> begin a.(0) |> 
                   function 
                   | N _ | Zero -> Error "11 digits must start with 1"
                   | One -> parse_num_array a ~start:1
                   | _ -> failwith "unreachable"
                 end
         | 10 -> parse_num_array a
         | n  -> if n > 11 
                 then Error "more than 11 digits"
                 else Error "incorrect number of digits")
       end


let number s =
    s |> String.to_array
      |> Array.map ~f:num_of_char
      |> verify_num_array
