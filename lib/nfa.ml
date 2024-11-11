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
  type t = (Character_rule.t option * State.t) iarray [@@deriving sexp_of]

  let empty = [::]
  let set_rule t (rule, state) = Iarray.append t [: Some rule, state :]
  let set_epsilon_exn t target = Iarray.append t [: None, target :]
  let exists t ~f = Iarray.exists t ~f:(fun (rule, state) -> f rule state)
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
      | Some rule -> Node.set_rule node (rule, to_state)
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

let eval_at t input ~offset =
  let rec eval_inner t input ~offset ~current_state =
    if State.equal current_state t.accepting_state
    then true
    else (
      let edges = Iarray.get t.nodes (State.to_int current_state) in
      Node.exists edges ~f:(fun rule target ->
        match rule with
        | None -> eval_inner t input ~offset ~current_state:target
        | Some rule ->
          (match Character_rule.matches rule ~input ~offset with
           | None -> false
           | Some consumed ->
             eval_inner t input ~offset:(offset + consumed) ~current_state:target)))
  in
  eval_inner t input ~current_state:State.zero ~offset
;;

let eval t input =
  let initial_edges = Iarray.get t.nodes 0 in
  match initial_edges with
  | [: (Some Start_of_line, _) :] -> eval_at t input ~offset:0
  | [: (Some (Literal l), _) :] ->
    (* Fast path: use kmp to find the start point *)
    With_return.with_return (fun { return } ->
      let pos = ref 0 in
      while
        match String.Search_pattern.index ~pos:!pos l ~in_:input with
        | Some offset ->
          if eval_at t input ~offset
          then return true
          else (
            pos := offset;
            true)
        | None -> false
      do
        ()
      done;
      false)
  | _ ->
    With_return.with_return (fun { return } ->
      for i = 0 to String.length input - 1 do
        if eval_at t input ~offset:i then return true
      done;
      false)
;;
