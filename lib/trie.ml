open! Core
open! Import
module Id = Unique_id.Int ()

(* Not a particularly fast implementation of a Trie, but intended to be used as
   part of the "building" phase of aho-corasick. *)

type t =
  { root_state : Id.t
  ; states : Id.t Char.Map.t Id.Table.t
  ; accepting_states : String.t list Id.Table.t
  }
[@@deriving sexp_of]

let create () =
  let t =
    { root_state = Id.of_int_exn 0
    ; states = Id.Table.create ()
    ; accepting_states = Id.Table.create ()
    }
  in
  Hashtbl.add_exn t.states ~key:t.root_state ~data:Char.Map.empty;
  t
;;

let fresh_state t =
  let id = Hashtbl.length t.states |> Id.of_int_exn in
  Hashtbl.add_exn t.states ~key:id ~data:Char.Map.empty;
  id
;;

let add_string t initial_string =
  let rec loop state_id string =
    match string with
    | [] -> Hashtbl.add_multi t.accepting_states ~key:state_id ~data:initial_string
    | hd :: tl ->
      let next_state_id =
        match
          Map.find
            (Hashtbl.find t.states state_id |> Option.value ~default:Char.Map.empty)
            hd
        with
        | None ->
          let next_state_id = fresh_state t in
          Hashtbl.update t.states state_id ~f:(fun map ->
            let map = Option.value ~default:Char.Map.empty map in
            Map.add_exn map ~key:hd ~data:next_state_id);
          next_state_id
        | Some next_state_id -> next_state_id
      in
      loop next_state_id tl
  in
  loop t.root_state (String.to_list initial_string)
;;

let%expect_test "trie building a trie" =
  let t = create () in
  add_string t "hello";
  add_string t "hallo";
  add_string t "world";
  Expect_test_helpers_core.print_s [%message (t : t)];
  [%expect
    {|
    (t (
      (root_state 0)
      (states (
        (0 ((h 1) (w 10)))
        (1 ((a 6) (e 2)))
        (2 ((l 3)))
        (3 ((l 4)))
        (4 ((o 5)))
        (5 ())
        (6 ((l 7)))
        (7 ((l 8)))
        (8 ((o 9)))
        (9 ())
        (10 ((o 11)))
        (11 ((r 12)))
        (12 ((l 13)))
        (13 ((d 14)))
        (14 ())))
      (accepting_states (
        (5  (hello))
        (9  (hallo))
        (14 (world))))))
    |}]
;;
