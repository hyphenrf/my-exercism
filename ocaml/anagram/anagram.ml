let primes = [|2;3;5;7;11;13;17;19;23;29;31;37;41;43
             ;47;53;59;61;67;71;73;79;83;89;97;101|]

let anagrams needle haystack =
    let chksum_and_lower s =
        let sum = ref 1 in
        let lo = ref [] in
        StringLabels.iter s ~f:(fun c ->
            begin
            sum := !sum * begin 
            Char.lowercase_ascii c |> Char.code
            |> (-) 0x7A |> Array.get primes end;
            lo := (Char.lowercase_ascii c)::!lo
            end);
        sum, lo
    in
    let (nsum, nlo) = chksum_and_lower needle in
    let len = String.length in
    ListLabels.filter haystack ~f:(fun s ->
        if len s <> len needle then false else
        match chksum_and_lower s with
            | (_, lo ) when lo  = nlo    -> false
            | (sum, _) when sum = nsum   -> true
            | _ -> false )

