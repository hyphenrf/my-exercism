module L = Int64

let factors_of n =
    let rec factorise ?(fact=2L) ?(facts=[]) = function
        | 1L -> List.rev facts
        | i when L.rem i fact = 0L ->
                factorise ~fact:fact ~facts:(fact::facts) (L.div i fact)
        | i ->  factorise ~fact:(L.add fact 1L) ~facts:facts i
    in factorise n
