let moves = [ (-1,-1); (-1, 0); (-1, 1)
            ; (0, -1); (*0, 0*) (0,  1)
            ; (1, -1); (1,  0); (1,  1)
            ]

let get row col board = 
    let (@:) a ar =
        if a < 0 || a >= Array.length ar  then None 
        else Some ar.(a)
    in
    let (@.) a st =
        if a < 0 || a >= Bytes.length st then None
        else Some (Bytes.get st a)
    in
    let (|=) = Option.bind in
    
    row @: board |= (@.) col

let bos = Bytes.of_string
let bump = function
    | '1'..'7' as a -> Char.chr @@int_of_char a + 1
    | ' ' -> '1'
    | a -> a

let count row col board =
    let surrounding (r, c) =
        match get (r+row) (c+col) !board with
        | Some a -> !board.(r+row).[c+col] <- (bump a) 
        | None -> ()
    in
    List.iter surrounding moves; !board



let annotate = function
    | [] -> [] 
    | [a] when String.length a < 2 -> [a]
    | initial ->
        let board = initial |> Array.of_list |> Array.map bos |> ref in
        
        !board |> Array.iteri (fun r str -> BytesLabels.iteri str
                          ~f:(fun c chr -> 
                              if chr = '*' 
                              then ignore (count r c board)));
        !board |> Array.to_list |> List.map Bytes.to_string
