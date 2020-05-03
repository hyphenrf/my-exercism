let sum ?(fn=(+)) l =
  List.fold_left fn 0 l
let sq a = a * a

let square_of_sum n =
  List.init n (fun a -> a + 1) |> sum |> sq

let sum_of_squares n =
  List.init n (fun a -> a + 1) |> sum ~fn:(fun acc x -> acc + (sq x))

let difference_of_squares n =
  (square_of_sum n) - (sum_of_squares n)
