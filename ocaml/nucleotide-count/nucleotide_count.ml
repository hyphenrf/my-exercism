open Base


exception NotNucl of char

let nucl = function
  | ('A' | 'G' | 'C' | 'T') as n -> n
  | x -> raise (NotNucl x)

let keyget cmap c = match Map.find cmap c with
  | Some d -> d
  | None   -> 0

let count_nucleotides s =
  let bump cmap c =
    Map.set cmap ~key:(nucl c) ~data:(keyget cmap c + 1) 
  in 
  let cmap = ref (Map.empty (module Char)) in 
  try String.iter s ~f:(fun c -> cmap := bump !cmap c);
    Ok !cmap
  with NotNucl x -> Error x

let count_nucleotide s c =
  try match count_nucleotides s with
    | Ok cmap -> Ok (keyget cmap (nucl c))
    | Error x -> Error x
  with
    NotNucl x -> Error x

