open Base
open Int64

let rec words = function
    | 0L -> "zero" | 10L -> "ten"
    | 1L -> "one"  | 11L -> "eleven"
    | 2L -> "two"  | 12L -> "twelve"   | 20L -> "twenty"
    | 3L -> "three"| 13L -> "thirteen" | 30L -> "thirty"
    | 4L -> "four" | 14L -> "fourteen" | 40L -> "forty"
    | 5L -> "five" | 15L -> "fifteen"  | 50L -> "fifty"
    | 6L -> "six"  | 16L -> "sixteen"  | 60L -> "sixty"
    | 7L -> "seven"| 17L -> "seventeen"| 70L -> "seventy"
    | 8L -> "eight"| 18L -> "eighteen" | 80L -> "eighty"
    | 9L -> "nine" | 19L -> "nineteen" | 90L -> "ninety"

    | n when n < 100L -> words (n - n % 10L) ^ "-" ^ words (n % 10L)
    | n when n < 1_000L -> words (n/100L) ^ " hundred" ^ with_rem n 100L
    | n when n < 1_000_000L -> words (n/1_000L) ^ " thousand" ^ with_rem n 1_000L
    | n when n < 1_000_000_000L -> words (n/1_000_000L) ^ " million" ^ with_rem n 1_000_000L
    | n -> words (n/1_000_000_000L) ^ " billion" ^ with_rem n 1_000_000_000L

and with_rem n m = let r = n % m in
                    if r = 0L
                    then ""
                    else " " ^ words r

let in_english n = if n < 0L || n > 999_999_999_999L
    then Error "input out of range" 
    else Ok (words n)
