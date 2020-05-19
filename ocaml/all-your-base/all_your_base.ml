type base = int

let convert_bases ~from ~digits ~target =
  if from < 2 || target < 2 then None
  else if not @@List.for_all (fun x -> x >= 0 && x < from) digits then None
  else
    let rec to_base_list = function
      | n when n = 0 -> []
      | n -> let (d, m) = (n / target), (n mod target) in
             (to_base_list d) @ [m]
    in
    let places = List.length digits in
    let powers = List.init places (fun x -> places - x - 1) in
    let ( ** ) a b = int_of_float (float a ** float b) in
    let out = List.fold_left2 
              (fun acc digit place -> acc + digit * (from ** place)) 
              0 digits powers
    |> to_base_list
    in if out = [] then Some [0] else Some out

