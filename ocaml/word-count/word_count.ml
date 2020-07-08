open Base

let sym_strip s = match 
    String.strip s ~drop:(fun c -> not @@ Char.is_alphanum c)
    with | "" -> None
         | cs -> Some cs

let bump = Map.update ~f:(function None -> 1 | Some n -> 1+n)

let word_count s = s
    |> String.lowercase
    |> String.split_on_chars ~on:['\n';' ';',']
    |> List.filter_map ~f:sym_strip
    |> List.fold ~f:bump ~init:(Map.empty (module String))
