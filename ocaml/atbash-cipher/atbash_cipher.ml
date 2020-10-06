open Base

let table = "0123456789zyxwvutsrqponmlkjihgfedcba"

let lookup = let open Char in
    function
    | '0'..'9' as n -> table.[to_int n land 0x0f]
    | 'a'..'z' | 'A'..'Z' as a -> table.[to_int a land 0x1f + 9]
    | _ -> assert false (* unreachable *)

let group block_size s = let open String in
    foldi s ~init:"" ~f:(fun i acc c ->
        if Int.( i % block_size = 0 )
        then acc ^ " " ^ Char.to_string c
        else acc ^ Char.to_string c)
    |> lstrip

let encode ?block_size s =
    let spaces = match block_size with
        | None -> 5 (* default *)
        | Some n -> n
    in
    s |> String.filter ~f:Char.is_alphanum
      |> String.map ~f:lookup
      |> group spaces

let decode s =
    s |> String.filter ~f:Char.is_alphanum
      |> String.map ~f:lookup
