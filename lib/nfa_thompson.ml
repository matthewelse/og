open! Core
open! Import

module State : sig
  type t : immediate64 [@@deriving compare, equal, sexp_of]

  val zero : t
  val succ : t -> t
  val to_int : t -> int

  include Comparable.S_plain with type t := t
end = struct
  include Int
end

module Node = struct
  type t =
    | Accept
    | Split of State.t * State.t
    | Match of Char.t * State.t
    | Jump of State.t
  [@@deriving sexp_of]
end

module Builder = struct
  type t =
    { global_ states : Node.t option Dynarray.t
    ; mutable next_state : State.t
    }
  [@@deriving sexp_of]

  let create () = { states = Dynarray.create (); next_state = State.zero }

  let fresh_state t =
    let state = t.next_state in
    t.next_state <- State.succ t.next_state;
    state
  ;;

  let set_state t state node =
    Dynarray.resize t.states (State.to_int state + 1) ~default:(Fn.const None);
    Dynarray.set t.states (State.to_int state) (Some node)
  ;;
end

type t = Node.t iarray [@@deriving sexp_of]

let eval _t _slice = todo ()

let build (local_ (f : Builder.t @ local -> State.t)) : t =
  let builder = Builder.create () in
  let accepting_state = f builder in
  Builder.set_state builder accepting_state Accept;
  let%tydi { states; _ } = builder in
  let states =
    Dynarray.to_list states
    |> List.map ~f:(fun node ->
      Option.value_exn ~message:"BUG: a node was not initialized" node)
    |> Iarray.of_list
  in
  states
;;

let compile (re : Regex0.t) =
  let rec aux (local_ builder) (re : Regex0.t) ~initial_state =
    match re with
    | String literal ->
      String.fold literal ~init:initial_state ~f:(fun initial_state c ->
        let next_state = Builder.fresh_state builder in
        Builder.set_state builder initial_state (Match (c, next_state));
        next_state) [@nontail]
    | Opt re ->
      let taken_state = Builder.fresh_state builder in
      let accepting_state = aux builder re ~initial_state:taken_state in
      Builder.set_state builder initial_state (Split (taken_state, accepting_state));
      accepting_state
    | Or (re1, re2) ->
      let left_taken_state = Builder.fresh_state builder in
      let left_accepting_state = aux builder re1 ~initial_state:left_taken_state in
      let right_taken_state = Builder.fresh_state builder in
      let right_accepting_state = aux builder re2 ~initial_state:right_taken_state in
      Builder.set_state
        builder
        initial_state
        (Split (left_taken_state, right_taken_state));
      Builder.set_state builder left_accepting_state (Jump right_accepting_state);
      right_accepting_state
    | Rep1 re ->
      let single_accepting_state = aux builder re ~initial_state in
      let accepting_state = Builder.fresh_state builder in
      Builder.set_state
        builder
        single_accepting_state
        (Split (accepting_state, initial_state));
      accepting_state
    | Seq res ->
      List.fold res ~init:initial_state ~f:(fun initial_state re ->
        aux builder re ~initial_state) [@nontail]
    | Start_of_line | Class _ | End_of_line | Neg_group _ | Group _ ->
      failwith "Not supported."
  in
  build (fun builder -> aux builder re ~initial_state:(Builder.fresh_state builder))
;;
