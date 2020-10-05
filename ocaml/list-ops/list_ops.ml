let rec fold ~init ~f = function
    | [] -> init
    | x::xs -> fold ~init:(f init x) ~f:f xs

let length l = fold l ~init:0 ~f:(fun acc _ -> acc+1)

let reverse l = fold l ~init:[] ~f:(fun xs x -> x::xs)

let map ~f l = fold l ~init:[] ~f:(fun xs x -> f x::xs)
    |> reverse

let filter ~f l = fold l ~init:[] 
    ~f:(fun xs x -> if f x then x::xs else xs) 
    |> reverse

let append l r = reverse l 
    |> fold ~init:r ~f:(fun xs x -> x::xs)

let concat l = fold ~init:[] ~f:append l
