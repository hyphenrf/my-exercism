let validate a b c =
  a+b > c && b+c > a && c+a > b

let is_equilateral a b c = validate a b c
  && a = b && b = c

let is_isosceles a b c   = validate a b c
  && (a = b || b = c || c = a)

let is_scalene a b c     = validate a b c
  && a <> b && b <> c
