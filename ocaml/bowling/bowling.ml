type t = { game: int list
         ; bowl: int
         ; frame: int
         }

let new_game = { game = []; bowl = 0; frame = 0 }

let roll pins {game;bowl;frame} =
  if frame = 9 && bowl > 2 then
    Error "Cannot roll after game is over"
  else if frame > 9 then
    Error "Cannot roll after game is over"
  else if frame = 9 && bowl = 2 && List.(hd game + hd (tl game) < 10) then
    Error "Cannot roll after game is over"
  else if pins < 0 then
    Error "Negative roll is invalid"
  else if pins > 10 then
    Error "Pin count exceeds pins on the lane"
  else if frame < 9 && bowl > 0 && List.hd game + pins > 10 then
    Error "Pin count exceeds pins on the lane"
  else if frame = 9 && bowl = 2 && List.hd game < 10 && List.hd game + pins > 10
  && List.(hd @@ tl game) = 10 then
    Error "Pin count exceeds pins on the lane"
  else begin
    let game = pins :: game in
    let frame, bowl =
      if frame < 9 then
        begin
        if pins = 10 && bowl = 0 then
          frame + 1, bowl
        else
          frame + bowl, succ bowl mod 2
        end
      else
        frame, succ bowl
    in
    Ok {frame;bowl;game} 
  end
    
let score {game;frame;bowl} = 
  if frame < 9 then
    Error "Score cannot be taken until the end of the game"
  else if frame = 9 && bowl < 3 && List.(hd game + hd (tl game) >= 10) then
    Error "Score cannot be taken until the end of the game"
  else begin
    let open Stream in
    let s = of_list @@ List.rev game in
    let score = ref 0 in
    for _ = 0 to 9 do
      let x = next s in
      if x = 10 then (* strike *)
        let j = npeek 2 s |> List.fold_left (+) 0 in
        score := !score + x + j
      else begin
        let j = next s in
        if x + j = 10 then (* spare *)
          let k = peek s |> Option.value ~default:0 in
          score := !score + x + j + k
        else (* open *)
          score := !score + x + j
      end
    done;
    Ok !score
  end
