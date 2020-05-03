
let raindrop n =
  match
    (if n mod 3 == 0 then "Pling" else "") ^
    (if n mod 5 == 0 then "Plang" else "") ^
    (if n mod 7 == 0 then "Plong" else "") 
  with  
    | ""   -> string_of_int n
    | rain -> rain

