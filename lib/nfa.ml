open! Core
open! Import

module State : sig
  type t : immediate64 [@@deriving compare, equal, sexp_of]

  val zero : t
  val succ : t -> t
  val to_int : t -> int

  include Comparable.S_plain with type t := t
end =
  Int

module Node = struct
  type t =
    { rule : (Character_rule.t * State.t) option
    ; epsilon_rule : State.t option
    }
  [@@deriving sexp_of]

  let empty = { rule = None; epsilon_rule = None }

  let set_rule_exn t rule =
    assert (Option.is_none t.rule);
    { t with rule = Some rule }
  ;;

  let set_epsilon_exn t target =
    assert (Option.is_none t.epsilon_rule);
    { t with epsilon_rule = Some target }
  ;;

  let _fold t ~init:acc ~f =
    let acc =
      match t.rule with
      | None -> acc
      | Some (rule, to_state) -> f acc (Some rule) to_state
    in
    match t.epsilon_rule with
    | None -> acc
    | Some to_state -> f acc None to_state
  ;;

  let exists t ~f =
    (match t.rule with
     | None -> false
     | Some (rule, to_state) -> f (Some rule) to_state)
    ||
    match t.epsilon_rule with
    | None -> false
    | Some to_state -> f None to_state
  ;;
end

module Builder = struct
  type t =
    { global_ states : Node.t Dynarray.t
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
    Dynarray.resize t.states (State.to_int from_state + 1) ~default:(Fn.const Node.empty);
    let node = Dynarray.get t.states (State.to_int from_state) in
    let node =
      match rule with
      | None -> Node.set_epsilon_exn node to_state
      | Some rule -> Node.set_rule_exn node (rule, to_state)
    in
    Dynarray.set t.states (State.to_int from_state) node
  ;;
end

type t =
  { nodes : Node.t iarray
  ; accepting_state : State.t
  }
[@@deriving sexp_of]

let build (f : Builder.t @ local -> State.t) =
  let builder = Builder.create () in
  let accepting_state = f builder in
  let%tydi { states; _ } = builder in
  let states = Iarray.unsafe_of_array (Dynarray.to_array states) in
  { nodes = states; accepting_state }
;;

let eval t input =
  let rec eval_inner t input ~offset ~current_state =
    if State.equal current_state t.accepting_state
    then true
    else (
      let edges = Iarray.get_exn t.nodes (State.to_int current_state) in
      Node.exists edges ~f:(fun rule target ->
        match rule with
        | None -> eval_inner t input ~offset ~current_state:target
        | Some rule ->
          (match Character_rule.matches rule ~input ~offset with
           | None -> false
           | Some consumed ->
             eval_inner t input ~offset:(offset + consumed) ~current_state:target)))
  in
  eval_inner t input ~current_state:State.zero
;;
