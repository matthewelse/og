open! Core
open! Import

module State : sig
  type t : immediate64 [@@deriving compare, equal, sexp_of]

  val zero : t
  val succ : t -> t
  val to_int : t -> int
end =
  Int

module Edge = struct
  type t =
    { rule : Character_rule.t
    ; target : State.t
    }
  [@@deriving sexp_of]
end

module Builder = struct
  type t =
    { global_ states : Edge.t list Dynarray.t
    ; mutable next_state : State.t
    }
  [@@deriving sexp_of]

  let create () = { states = Stdlib.Dynarray.create (); next_state = State.zero }

  let fresh_state t =
    let state = t.next_state in
    t.next_state <- State.succ t.next_state;
    state
  ;;

  let add_edge t ~from_state ~to_state rule =
    (* TODO: require that rules for the same edge don't overlap. *)
    Dynarray.resize t.states (State.to_int from_state + 1) ~default:(Fn.const []);
    let edges = Dynarray.get t.states (State.to_int from_state) in
    Dynarray.set t.states (State.to_int from_state) ({ rule; target = to_state } :: edges)
  ;;
end

type t =
  { nodes : Edge.t iarray iarray
  ; accepting_state : State.t
  }
[@@deriving sexp_of]

let build (f : Builder.t @ local -> State.t) =
  let builder = Builder.create () in
  let accepting_state = f builder in
  let%tydi { states; _ } = builder in
  let states =
    Iarray.unsafe_of_array (Dynarray.to_array states) |> Iarray.map ~f:Iarray.of_list
  in
  { nodes = states; accepting_state }
;;

let eval t input =
  let rec eval_inner t input ~offset ~current_state =
    if State.equal current_state t.accepting_state
    then true
    else (
      let edges = Iarray.get_exn t.nodes (State.to_int current_state) in
      let edge =
        Iarray.find_map_local edges ~f:(fun { rule; target } ->
          match Character_rule.matches rule ~input ~offset with
          | None -> None
          | Some consumed -> exclave_ Some (consumed, target))
      in
      match edge with
      | None -> false
      | Some (consumed, target) ->
        (eval_inner [@tailcall]) t input ~offset:(offset + consumed) ~current_state:target)
  in
  eval_inner t input ~current_state:State.zero
;;
