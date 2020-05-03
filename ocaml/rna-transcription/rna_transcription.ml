type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

(* Polymorphic Variants; Define non-unique (usually enum) types
 * SEE: https://archive.vn/62BHg *)

let to_rna = List.map ( function `G -> `C
                               | `C -> `G
                               | `T -> `A
                               | `A -> `U )
