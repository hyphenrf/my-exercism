type t = { game: int list; bowl: int }

let new_game = { game = []; bowl = 0 }


let last2 = function
  | x::y::_ -> x, y
  | x::_ -> x, 0
  | [] -> 0, 0
let odd x = x land 1 = 1
let even x = not (odd x)

let roll pins {game;bowl} =
  let x, y = last2 game in
  let game = pins::game in

  if pins < 0 then
    Error "Negative roll is invalid"
  else
  if pins > 10
  || bowl < 18 && odd bowl && x + pins > 10
  || bowl = 20 && y = 10 && x < 10 && x + pins > 10
  then
    Error "Pin count exceeds pins on the lane"
  else
  if bowl > 20
  || bowl = 20 && x + y < 10
  then
    Error "Cannot roll after game is over"
  else
  if pins = 10 && even bowl && bowl < 18 then
    Ok { bowl = bowl + 2; game }
  else
    Ok { bowl = bowl + 1; game }


let score {game;_} =
  let rec aux (frame, score) game =
    if frame < 11 then
      match game with
      | 10::(y::z::_ as rest)                -> aux (frame+1, score+10+y+z) rest
      | x::y::(z::_ as rest) when x + y = 10 -> aux (frame+1, score+10+z) rest
      | x::y::rest           when x + y < 10 -> aux (frame+1, score+x+y) rest
      | _::_ | [] -> Error "Score cannot be taken until the end of the game"
    else
      Ok score
  in aux (1, 0) @@ List.rev game
