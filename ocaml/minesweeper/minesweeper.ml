let getitem (row, col) board =
    try       Some Bytes.(get board.(row) col)
    with _ -> None

let cells = [ (-1,-1); (-1, 0); (-1, 1)
            ; (0, -1); (*0, 0*) (0,  1)
            ; (1, -1); (1,  0); (1,  1) ]

let bump = function
    | '1'..'7' as c -> Char.(chr (code c + 1))
    | ' ' -> '1'
    | c -> c

let annotate ls =
    let board = ls |> Array.of_list
                   |> Array.map Bytes.of_string
    in
    let flag rmine cmine =
        let surround (r, c) =
            match getitem (r+rmine, c+cmine) board with
            | Some a ->
                board.(r+rmine).[c+cmine] <- bump a
            | None -> ()
        in
        List.iter surround cells
    in
    board |> Array.iteri
             (fun r -> Bytes.iteri
             (fun c chr -> if chr = '*' then flag r c));

             board |> Array.map Bytes.to_string
                   |> Array.to_list
