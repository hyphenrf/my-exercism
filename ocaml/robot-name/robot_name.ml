(**
 *  This runs as expected and passes all (including optional) tests.
 *  Mutable records are great for encoding state without passing params
 *  everywhere.
 *)

module BSet = Set.Make (Bytes)
type robot = { mutable id: bytes }

let known_ids = ref BSet.empty
let know id = BSet.mem id !known_ids
let remove id = BSet.remove id !known_ids
let remember id = BSet.add id !known_ids

let rec new_robot () =
    let rand_id = Bytes.init 5 @@
        fun x -> if x < 2
                 then Char.chr @@ (Random.int 26) + 0x41 (*A-Z*)
                 else Char.chr @@ (Random.int 10) + 0x30 (*0-9*)
    in 
    if know rand_id then new_robot()
    else (known_ids := remember rand_id;
         {id = rand_id})

let name {id} =
    Bytes.to_string id

let reset old_robot =
    (* we need to call new before removing old
     * to avoid the tiny chance of duplication
     *)
    let new_robot = new_robot ()
    in
    known_ids := remove old_robot.id;
    old_robot.id <- new_robot.id
