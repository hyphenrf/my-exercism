(* This doesb't work because robot's name never changes once it is assigned 
 * Reset doesn't MUTATE the current robot's name *)
type robot = string
module RobotSet = Set.Make (String)

let robots = ref RobotSet.empty
let () = Random.self_init ()


let rec new_robot () =
    let name =
        String.init 5 (fun x -> if x < 2
                      then Char.chr @@ (Random.int 0x1a) + 0x41
                      else Char.chr @@ (Random.int 0x0a) + 0x30)
    in
    if RobotSet.mem name !robots then new_robot ()
    else (robots := RobotSet.add name !robots; name)


let name (r:robot) = r

let reset robot =
    robots := RobotSet.remove robot !robots
