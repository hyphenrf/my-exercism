open Base

(* Quality of life *)
let ( >>= ) = Option.( >>= )
let ( let* ) = ( >>= )

(* stack *)
let s = ref []

let push x = s := x::!s (* X *)

let peek () = List.hd !s

let pop () =
  let* x = peek () in
  s := List.tl_exn !s;
  Some x
(* X: the expected return in tests is a reversed stack. So we must eventually
      reverse this stack on evaluation. This is just a formatting choice. *)

(* builtin words as functions of type word = unit -> unit option *)
let dup () = let* x = peek () in Some (push x)

let drop () = let* _ = pop () in Some ()

let swap () =
  let* a = pop () in
  let* b = pop () in
  push a; push b;
  Some ()

let over () =
  let* a = pop () in
  let* b = peek () in
  push a; push b;
  Some ()

let binop f () =
  let* a = pop () in
  let* b = pop () in
  match f b a with exception _ -> None | n -> Some (push n)

(* dict of word definitions: (string, word) Hashtbl.t *)
let words = Hashtbl.create (module String)

let get word = Hashtbl.find words word

let set word def = Hashtbl.set words ~key:word ~data:def

let kwinit () =
  set "+" (binop ( + ));
  set "-" (binop ( - ));
  set "/" (binop ( / ));
  set "*" (binop ( * ));
  set "dup" dup;
  set "drop" drop;
  set "swap" swap;
  set "over" over

(* tokenizing *)
type word = unit -> unit option

type token = Num of int
           | Wrd of word option

(* forth words are separated by 'space' which we define as any char <= 0x20 *)
let whitespace = List.init 0x21 ~f:Char.of_int_exn

(* forth is case-insensitive *)
let lex s = String.(lowercase s |> split_on_chars ~on:whitespace)

let identify lexm =
  Caml.int_of_string_opt lexm (* TODO: Caml module :( *)
  |> function None -> Wrd (get lexm) (* X *)
            | Some n -> Num n
(* X: important to get word definition here in this early stage so that a word
      redefinition works and doesn't recurse indefinitely *)

(* with this and the function above we can identify one or many *)
let tokens = List.map ~f:identify

(* custom foldM for bailing on bad computations (represented as None) *)
let fold_opt ~f:fn =
  List.fold_left ~init:(Some ())
    ~f:(fun opt x -> match opt with None -> None 
                                  | Some _ -> fn x)

let run =
  fold_opt ~f:(function Num n -> Some (push n)
                      | Wrd w -> w >>= fun f -> f ())

let compile = function
  | _ :: word :: body ->
    begin
      match identify word with
      | Num _ -> None
      | _ ->
          let* def = List.drop_last body in (* get rid of the semicolon *)
          let toks = tokens def in          (* X *)
          set word (fun () -> run toks);
          Some ()
    end
  | _ -> None
(* x: important to compute this here, and not inside the thunk, to avoid 
      self-recursion *)

let reset () =
  s := [];
  Hashtbl.clear words;
  kwinit ()

let evaluate cmds =
  reset (); (* important between evals *)
  fold_opt cmds ~f:(
    fun cmd ->
      lex cmd |> function
        | [] -> Some ()
        | x :: _ as lexs ->
          if String.(x = ":") then compile lexs else run (tokens lexs)
  ) >>= fun () -> Some (List.rev !s)
