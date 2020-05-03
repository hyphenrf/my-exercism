type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let allergic_to num allergy =
  let int_of_allergen = function
  | Eggs     -> 1   | Peanuts   -> 2  | Shellfish -> 4  | Strawberries -> 8
  | Tomatoes -> 16  | Chocolate -> 32 | Pollen    -> 64 | Cats       -> 128
  in
    num land int_of_allergen allergy <> 0

let allergies num =
  List.filter (allergic_to num)
  [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats]
