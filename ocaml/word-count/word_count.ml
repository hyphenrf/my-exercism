module Map = Base.Map
module String = Base.String

let is_wordchar = function
    | 'a'..'z'
    | '0'..'9' -> true
    | _        -> false

let is_apos l m r = 
    m = '\''
    && is_wordchar l
    && is_wordchar r

let bump map word =
    let keyget = match Map.find map word with
        | Some n -> n
        | None   -> 0
    in
    Map.set map ~key:word ~data:(keyget + 1)


let word_count s =
    let s = "==" ^ (String.lowercase s) ^ "==" in
    let smap = ref (Map.empty (module String)) in
    let stmp = ref "" in

    for i = 1 to (String.length s) - 2 do

        let l, m, r = s.[i-1], s.[i], s.[i+1] 
        in
        if is_wordchar m || is_apos l m r
        then stmp := (!stmp ^ String.make 1 m)
        else match !stmp with
             | "" -> ()
             | _  -> smap := bump !smap !stmp;
                     stmp := ""
    done; !smap
