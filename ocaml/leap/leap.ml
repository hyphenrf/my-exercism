(* util *)
let (~!) = not
let (%?) a b =
  a mod b = 0 

(* solution *)
let leap_year y =
  (y %? 4) && 
    ~!(y %? 100) ||
          (y %? 400)
