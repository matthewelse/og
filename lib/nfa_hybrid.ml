open! Core
open! Import

module State : sig
  type t : immediate64 [@@deriving compare, equal, sexp_of]

  val zero : t
  val succ : t -> t
  val to_int : t -> int
  val of_int_exn : int -> t

  include Comparable.S_plain with type t := t
end = struct
  include Int
end

module Node = struct
  type t =
    | Accept
    | Split of State.t * State.t
    | Match of Charset.t * State.t
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

module DState : sig
  type t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val create : State.t list -> t
  val fold : t -> init:'acc -> f:('acc -> State.t -> 'acc) @ local -> 'acc
  val mem : t -> State.t -> bool
end = struct
  type t = Bitset.t [@@deriving compare, hash, sexp_of]

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain

  let create states = Bitset.of_list (List.map states ~f:State.to_int)

  let fold t ~init ~f =
    Bitset.fold t ~init ~f:(fun acc i -> f acc (State.of_int_exn i)) [@nontail]
  ;;

  let mem t state = Bitset.mem t (State.to_int state)
end

type t =
  { nfa : Node.t iarray
  ; cache : DState.t option Charmap.t DState.Table.t
  ; accepting_state : State.t
  ; flags : Flags.t
  ; look_for_constant : Slice.Search_pattern.t option
  }
[@@deriving sexp_of]

let build ~flags ~look_for_constant (local_ (f : Builder.t @ local -> State.t)) : t =
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
  { nfa = states
  ; cache = DState.Table.create ()
  ; accepting_state
  ; flags
  ; look_for_constant
  }
;;

let compile (re : Regex0.t) =
  let rec aux (local_ builder) (re : Regex0.Rule.t) ~initial_state =
    match re with
    | String literal ->
      String.fold literal ~init:initial_state ~f:(fun initial_state c ->
        let next_state = Builder.fresh_state builder in
        Builder.set_state
          builder
          initial_state
          (Match (Charset.of_list [ c ], next_state));
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
    | Class c ->
      let next_state = Builder.fresh_state builder in
      Builder.set_state
        builder
        initial_state
        (Match
           ( Char.all |> List.filter ~f:(Character_class.matches c) |> Charset.of_list
           , next_state ));
      next_state
    | Neg_group g ->
      let next_state = Builder.fresh_state builder in
      Builder.set_state
        builder
        initial_state
        (Match
           ( Char.all
             |> List.filter ~f:(fun c -> not (List.mem g ~equal:Char.equal c))
             |> Charset.of_list
           , next_state ));
      next_state
    | Group g ->
      let next_state = Builder.fresh_state builder in
      Builder.set_state
        builder
        initial_state
        (Match
           ( Char.all
             |> List.filter ~f:(fun c -> List.mem g ~equal:Char.equal c)
             |> Charset.of_list
           , next_state ));
      next_state
  in
  build
    ~flags:re.flags
    ~look_for_constant:
      (Option.map
         ~f:(fun str -> Slice.Search_pattern.create (Slice.create str))
         (Regex0.best_constant re))
    (fun builder -> aux builder re.re ~initial_state:(Builder.fresh_state builder))
;;

let unsafe_get_state (t : t) state = Iarray.unsafe_get t.nfa (State.to_int state)

let find_or_add_dstate t dstate =
  Hashtbl.find_or_add t.cache dstate ~default:(fun () -> Charmap.create None)
;;

let rec add_state t next_state acc ~generation ~state_generations =
  if state_generations.(State.to_int next_state) = !generation
  then acc
  else (
    match unsafe_get_state t next_state with
    | Jump s -> add_state t s acc ~generation ~state_generations
    | Split (s1, s2) ->
      let acc = add_state t s1 acc ~generation ~state_generations in
      add_state t s2 acc ~generation ~state_generations
    | _ -> next_state :: acc)
;;

let rec eval_inner t (local_ input) ~offset ~dstate ~generation ~state_generations =
  let open Charmap in
  generation := !generation + 1;
  let dnode = find_or_add_dstate t dstate in
  (DState.mem dstate t.accepting_state
   && ((not (Flags.mem t.flags Require_eol)) || offset = Slice.length input))
  ||
  match Slice.at input offset with
  | None -> false
  | Some c ->
    let dstate =
      match dnode.:(c) with
      | Some dstate -> dstate
      | None ->
        let nlist =
          DState.fold dstate ~init:[] ~f:(fun acc state ->
            (* TODO melse: safety assertion *)
            let rule = unsafe_get_state t state in
            match rule with
            | Match (cs, next_state) ->
              if Charset.mem cs c
              then add_state t next_state acc ~generation ~state_generations
              else acc
            | _ -> acc)
        in
        let dstate' = DState.create nlist in
        (* cache the state transition for next time *)
        dnode.:(c) <- Some dstate';
        dstate'
    in
    eval_inner t input ~offset:(offset + 1) ~dstate ~generation ~state_generations
;;

let eval t input =
  let maybe_contains_constant =
    match t.look_for_constant with
    | None -> true
    | Some pattern -> Slice.Search_pattern.index pattern input |> Option.is_some
  in
  let generation = ref 0 in
  let state_generations = Array.create_local ~len:(Iarray.length t.nfa) (-1) in
  maybe_contains_constant
  &&
  let dstate = DState.create (add_state t State.zero [] ~generation ~state_generations) in
  if Flags.mem t.flags Require_sol
  then eval_inner t input ~offset:0 ~dstate ~generation ~state_generations [@nontail]
  else
    With_return.with_return (fun { return } ->
      for offset = 0 to Slice.length input - 1 do
        if eval_inner t input ~offset ~dstate ~generation ~state_generations
        then return true
      done;
      false) [@nontail]
;;
