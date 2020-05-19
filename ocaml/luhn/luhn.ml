let valid sn = match String.trim sn with
  | s when String.length s < 2 -> false
  | s ->
    let cint c = Char.code c - 0x30 in
    let luhn n = if n > 4 then n * 2 - 9 else n * 2 in
    let even n = n land 1 = 0 in
    let rec check ?(i=String.length s) ?(n=1) ?(a=0) () = 
        match i with
        | 0 -> Some a
        | _ ->
          begin match s.[i-1] with
          | ' ' -> check ~i:(i-1) ~n:n ~a:a ()
          | '0'..'9' as c -> if even n 
              then check ~i:(i-1) ~n:(n+1) ~a:(a + luhn (cint c)) ()
              else check ~i:(i-1) ~n:(n+1) ~a:(a + cint c) ()
          | _ -> None 
          end
    in match check() with
    | Some a -> a mod 10 = 0
    | None   -> false
