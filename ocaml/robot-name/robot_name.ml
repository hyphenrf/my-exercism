(*  This runs as expected and passes all (including optional) tests.
 *  Mutable records are great for encoding state without passing params
 *  everywhere.
 *  Hash_set is running 30-50% faster than Set thanks to the fact that it's a
 *  table not a BST internally. I think it depends on whether or not the set is
 *  allowed to grow (collisions)
 *)
open Base
type robot = { mutable id: string }

let known_ids = Hash_set.create ~size:(26*26*1000) ~growth_allowed:true
                (module String)

let know = Hash_set.mem known_ids
let forget = Hash_set.remove known_ids
let remember = Hash_set.add known_ids


let rec new_robot () =
    let chr = Char.unsafe_of_int in
    let rand_id = String.init 5 ~f:(fun x ->
        if x < 2 then chr @@ Random.int 26 + 0x41
                 else chr @@ Random.int 10 + 0x30)
    in
    if know rand_id then new_robot()
    else (remember rand_id;
         {id = rand_id})

let name {id} = id

(* we need to call new before removing old to avoid the
 * tiny chance of duplication *)
let reset old_robot = let new_robot = new_robot () in
    forget old_robot.id;
    old_robot.id <- new_robot.id
