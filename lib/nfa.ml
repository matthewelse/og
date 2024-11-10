open! Base
open! Import
module State = Int

module Edge = struct
  type t =
    { rule : Character_rule.t option
    ; target : State.t
    }
  [@@deriving sexp_of]
end

module Node = struct
  type t = { edges : Edge.t list } [@@deriving sexp_of]
end

module Builder = struct
  type t =
    { states : Node.t Dynarray.t
    ; mutable next_state : State.t
    }
  [@@deriving sexp_of]

  let create () = { states = Stdlib.Dynarray.create (); next_state = State.zero }

  let fresh_state t =
    let state = t.next_state in
    t.next_state <- t.next_state + 1;
    state
  ;;

  let add_edge t ~from_state ~to_state rule =
    Dynarray.resize t.states (from_state + 1) ~default:(fun () -> { edges = [] });
    let%tydi { edges } = Dynarray.get t.states from_state in
    Dynarray.set t.states from_state { edges = { rule; target = to_state } :: edges }
  ;;
end

type t =
  { nodes : Node.t Dynarray.t
  ; accepting_state : State.t
  }
[@@deriving sexp_of]

let build f =
  let builder = Builder.create () in
  let accepting_state = f builder in
  { nodes = builder.states; accepting_state }
;;

let eval t input =
  let rec eval_inner t input ~offset ~current_state =
    if current_state = t.accepting_state
    then true
    else (
      let%tydi { edges } = Dynarray.get t.nodes current_state in
      List.exists edges ~f:(fun { rule; target } ->
        match rule with
        | None -> eval_inner t input ~offset ~current_state:target
        | Some rule ->
          (match Character_rule.matches rule ~input ~offset with
           | None -> false
           | Some consumed ->
             eval_inner t input ~offset:(offset + consumed) ~current_state:target)))
  in
  eval_inner t input ~current_state:0
;;
