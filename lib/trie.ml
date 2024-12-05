open! Core
open! Import

module Id : sig
  type t : immediate64 [@@deriving compare, equal, sexp_of]

  include Hashable.S_plain with type t := t
  include Intable.S with type t := t

  val to_string : t -> string
end =
  Int

let root_state = Id.of_int_exn 0

module Builder = struct
  type t =
    { states : Id.t Char.Map.t Id.Table.t
    ; accepting_states : Id.Hash_set.t
    }
  [@@deriving sexp_of]

  let create () =
    let t = { states = Id.Table.create (); accepting_states = Id.Hash_set.create () } in
    Hashtbl.add_exn t.states ~key:root_state ~data:Char.Map.empty;
    t
  ;;

  let fresh_state t =
    let id = Hashtbl.length t.states |> Id.of_int_exn in
    Hashtbl.add_exn t.states ~key:id ~data:Char.Map.empty;
    id
  ;;

  let state t id = Hashtbl.find_exn t.states id

  module type Stringy = sig
    type t

    val iter : t @ local -> f:(char -> unit) @ local -> unit
  end

  let update_state t id ~f =
    Hashtbl.update t.states id ~f:(fun state ->
      let state = Option.value ~default:Char.Map.empty state in
      f state)
  ;;

  let add_stringy
    (type s)
    (module Stringy : Stringy with type t = s)
    t
    (initial_string : s)
    =
    let current_state = ref root_state in
    Stringy.iter initial_string ~f:(fun this_char ->
      let next_state_id =
        match Map.find (state t !current_state) this_char with
        | None ->
          let next_state_id = fresh_state t in
          update_state
            t
            !current_state
            ~f:(Map.add_exn ~key:this_char ~data:next_state_id);
          next_state_id
        | Some next_state_id -> next_state_id
      in
      current_state := next_state_id);
    Hash_set.strict_add_exn t.accepting_states !current_state
  ;;
end

(* [0,255] are transitions, [256] indicates whether or not this is an accepting state. *)
type t = int iarray iarray [@@deriving sexp_of]

let accepting_state_idx = 256

let of_stringy
  (type s)
  (module Stringy : Builder.Stringy with type t = s)
  (local_ (strings : s list))
  : t
  =
  let builder = Builder.create () in
  List.iter_local_input strings ~f:(Builder.add_stringy (module Stringy) builder);
  let num_states = Hashtbl.length builder.states in
  let array =
    Array.init num_states ~f:(fun state_idx ->
      let state_id = Id.of_int_exn state_idx in
      let state_transitions = Builder.state builder state_id in
      Iarray.construct ~len:(accepting_state_idx + 1) ~default:(-1) ~f:(fun array ->
        for i = 0 to 255 do
          (* SAFETY: i >= 0, i <= 255 *)
          let c = Char.unsafe_of_int i in
          let next_state =
            match Map.find state_transitions c with
            | None -> root_state
            | Some next_state -> next_state
          in
          Array.unsafe_set array i (Id.to_int_exn next_state)
        done;
        if Hash_set.mem builder.accepting_states state_id
        then array.(accepting_state_idx) <- 1
        else array.(accepting_state_idx) <- 0))
  in
  Iarray.unsafe_of_array array
;;

let of_strings =
  of_stringy
    (module struct
      include String

      let iter t ~f =
        for i = 0 to length t - 1 do
          f (unsafe_get t i)
        done
      ;;
    end)
;;

let of_slices =
  of_stringy
    (module struct
      include Slice

      let iter t ~f = iter t ~f
    end)
;;

let is_accepting_state (local_ t) id =
  let idx = Id.to_int_exn id in
  (* SAFETY: [t.(n)] always contains [accepting_state_idx + 1] elements. *)
  Iarray.unsafe_get (Iarray.get t idx) accepting_state_idx = 1
;;

let next_state (local_ (t : t)) id char =
  let idx = Id.to_int_exn id in
  let this_state = Iarray.get t idx in
  let char_idx = Char.to_int char in
  Iarray.unsafe_get this_state char_idx |> Id.of_int_exn
;;

let iter_states t ~f =
  for i = 0 to Iarray.length t - 1 do
    f (Id.of_int_exn i)
  done
;;

let num_states t = Iarray.length t

let iter_children t id ~f =
  let idx = Id.to_int_exn id in
  let tbl = Iarray.get t idx in
  for i = 0 to 255 do
    (* SAFETY: i >= 0 && i <= 255 *)
    let c = Char.unsafe_of_int i in
    (* SAFETY: all entries have 256 elements. i >= 0 && i <= 255 *)
    let child = Iarray.unsafe_get tbl i in
    if child <> Id.to_int_exn root_state then f c (Id.of_int_exn child)
  done
;;

let%expect_test "trie building a trie" =
  let t = of_strings [ "hello"; "hallo"; "world"; "helloworld" ] in
  iter_states t ~f:(fun id ->
    let bullet = if is_accepting_state t id then "*" else "-" in
    print_endline [%string "%{bullet} id:%{id#Id}"];
    iter_children t id ~f:(fun c next_state ->
      print_endline [%string " %{c#Char} -> %{next_state#Id}"]);
    print_endline "");
  [%expect
    {|
    - id:0
     h -> 1
     w -> 10

    - id:1
     a -> 6
     e -> 2

    - id:2
     l -> 3

    - id:3
     l -> 4

    - id:4
     o -> 5

    * id:5
     w -> 15

    - id:6
     l -> 7

    - id:7
     l -> 8

    - id:8
     o -> 9

    * id:9

    - id:10
     o -> 11

    - id:11
     r -> 12

    - id:12
     l -> 13

    - id:13
     d -> 14

    * id:14

    - id:15
     o -> 16

    - id:16
     r -> 17

    - id:17
     l -> 18

    - id:18
     d -> 19

    * id:19
    |}]
;;
