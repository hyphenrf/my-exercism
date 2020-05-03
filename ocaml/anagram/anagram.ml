let anagrams needle haystack =
  let fixup s =
       List.init (String.length s) (String.get s)
    |> List.sort (Char.compare)
  in
  let needle  = String.lowercase_ascii needle in
  let needlef = fixup needle in
  List.filter (fun a -> match String.lowercase_ascii a with
                 | s when needle  = s -> false
                 | s when needlef = fixup s -> true
                 | _ -> false ) haystack

