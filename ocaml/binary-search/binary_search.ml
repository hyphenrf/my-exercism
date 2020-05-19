let find haystack needle = 
    let rec search l r =
        if l > r then Error "value not in array"
        else let i = (l+r)/2 in 
        match haystack.(i) with        
        | x when x = needle -> Ok i
        | x when x > needle -> search l (i-1)
        | x when x < needle -> search (i+1) r
    in 
    search 0 @@ Array.length haystack - 1
