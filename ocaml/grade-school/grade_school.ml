open Base

module Int_map = Map.M(Int)
type school = string list Int_map.t
let empty_school = Map.empty (module Int)


let grade g smap =
  match Map.find smap g with
      | Some names -> names
      | None -> []

let add name g smap =
  Map.set smap ~key:g ~data:(name::(grade g smap))

let sorted smap =
  Map.map smap ~f:(List.sort ~compare:(String.compare))

let roster smap =
  sorted smap |> Map.data |> List.concat

