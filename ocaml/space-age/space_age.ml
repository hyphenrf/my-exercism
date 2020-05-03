type planet = Mercury | Venus | Earth | Mars
            | Jupiter | Saturn | Neptune | Uranus

let age_on spacerock secs =
  let ey = float secs /. 31557600. in
  match spacerock with
      | Earth   -> ey           | Jupiter -> ey/.11.862615
      | Mercury -> ey/.0.240846 | Saturn  -> ey/.29.447498
      | Venus   -> ey/.0.615197 | Neptune -> ey/.164.79132
      | Mars    -> ey/.1.880815 | Uranus  -> ey/.84.016846

