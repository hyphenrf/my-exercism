module CS = Set.Make(Char)

let is_pangram s =
  String.lowercase_ascii s
  |> String.to_seq
  |> Seq.filter (fun x -> 96 < Char.code x && Char.code x < 123)
  |> CS.of_seq
  |> CS.cardinal = 26

