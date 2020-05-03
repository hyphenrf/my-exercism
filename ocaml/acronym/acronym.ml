let acronym sentence = 
  sentence 
  |> String.map (function ' '|'-'|'_' -> ':' | x -> x)
  |> String.split_on_char ':'
  |> List.map (function "" -> "" 
                      | s  -> Char.uppercase_ascii s.[0] |> String.make 1)
  |> String.concat ""
