open Base

let encode s =
    s |> String.to_list
      |> List.group ~break:Char.(<>)
      |> List.map ~f:(fun cs -> (match List.length cs with
                                    | 1 -> ""
                                    | n -> Int.to_string n) ^ 
                                Char.to_string (List.hd_exn cs))
      |> String.concat


let decode s = let open Str in
    let rec expand ?(s="") xs =
        match xs with
        | a::b::cs ->
                begin match a, b with 
                  (*text = numbers, delim = letter*)
                  | Text a, Delim b -> 
                          expand cs ~s:(s^String.make 
                                          (Int.of_string a) 
                                          (Char.of_string b))
                  | Delim a, Text b ->
                          expand (Text b::cs) ~s:(s^a)
                  | Delim a, Delim b ->
                          expand cs ~s:(s^a^b)
                end
        | [Text a] | [Delim a] -> s^a
        | [] -> s
    in
    s |> full_split (regexp_case_fold "[a-z ]")
      |> expand

