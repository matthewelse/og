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
  let exists t ~f = Iarray.exists t ~f:(fun (rule, state) -> f rule state) [@nontail]
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

let[@inline] node t state = Iarray.get t.nodes (State.to_int state)
let initial_state _t = State.zero
let accepting_state t = t.accepting_state

let compile (re : Regex0.t) =
  let rec to_nfa_inner builder (re : Regex0.t) ~current_state =
    match re with
    | Start_of_line ->
      let accepting_state = Builder.fresh_state builder in
      Builder.add_edge
        builder
        ~from_state:current_state
        ~to_state:accepting_state
        (Some Start_of_line);
      accepting_state
    | End_of_line ->
      let accepting_state = Builder.fresh_state builder in
      Builder.add_edge
        builder
        ~from_state:current_state
        ~to_state:accepting_state
        (Some End_of_line);
      accepting_state
    | String s ->
      let accepting_state = Builder.fresh_state builder in
      Builder.add_edge
        builder
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Literal (Slice.Search_pattern.create (Slice.create s))));
      accepting_state
    | Class c ->
      let accepting_state = Builder.fresh_state builder in
      Builder.add_edge
        builder
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Class c));
      accepting_state
    | Group g ->
      let accepting_state = Builder.fresh_state builder in
      Builder.add_edge
        builder
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (One_of (Iarray.of_list g)));
      accepting_state
    | Neg_group g ->
      let accepting_state = Builder.fresh_state builder in
      Builder.add_edge
        builder
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Not_one_of (Iarray.of_list g)));
      accepting_state
    | Opt re ->
      let accepting_state = to_nfa_inner builder re ~current_state in
      Builder.add_edge builder ~from_state:current_state ~to_state:accepting_state None;
      accepting_state
    | Or (first, second) ->
      let first_accepting_state = to_nfa_inner builder first ~current_state in
      let second_accepting_state = to_nfa_inner builder second ~current_state in
      Builder.add_edge
        builder
        ~from_state:second_accepting_state
        ~to_state:first_accepting_state
        None;
      first_accepting_state
    | Rep1 re ->
      let accepting_state = to_nfa_inner builder re ~current_state in
      Builder.add_edge builder ~from_state:accepting_state ~to_state:current_state None;
      accepting_state
    | Seq res ->
      List.fold res ~init:current_state ~f:(fun current_state re ->
        to_nfa_inner builder re ~current_state) [@nontail]
  in
  build (fun builder ->
    to_nfa_inner builder re ~current_state:(Builder.fresh_state builder) [@nontail])
;;
