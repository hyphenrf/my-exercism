type t = { game: int list; bowl: int; frame: int }

let new_game = { game = []; bowl = 0; frame = 0 }

let last2 xs =
  (try List.hd xs with _ -> 0),
  (try List.(hd @@ tl xs) with _ -> 0)

let roll pins {game;bowl;frame} =
  if pins < 0 then
    Error "Negative roll is invalid"
  else
  let x, y = last2 game in
  let game = pins :: game in

  if frame > 9 || bowl > 2 || frame = 9 && bowl = 2 && x + y < 10 then
    Error "Cannot roll after game is over"
  else if pins > 10 || frame < 9 && bowl > 0 && x + pins > 10 || frame = 9 && bowl = 2 && x < 10 && x + pins > 10 && y = 10 then
    Error "Pin count exceeds pins on the lane"
  else
    let frame, bowl =
      if frame = 9 then frame, succ bowl
      else if pins = 10 && bowl = 0 then frame+1, 0
      else frame + bowl, succ bowl mod 2
    in
    Ok {frame;bowl;game}

let score {game;_} =
  let rec aux (frame, score) =
    if frame < 11 then function
    | x::(y::z::_ as rest) when x = 10 -> aux (frame+1, score + 10 + y + z) rest
    | x::y::(z::_ as rest) when x + y = 10 -> aux (frame+1, score + 10 + z) rest
    | x::y::rest when x + y < 10 -> aux (frame+1, score + x + y) rest
    | _::_ ->
        Error "Score cannot be taken until the end of the game"
    | [] when frame < 11 ->
        Error "Score cannot be taken until the end of the game"
    | [] ->
        Ok score
    else Fun.const (Ok score)
  in aux (1, 0) (List.rev game)
