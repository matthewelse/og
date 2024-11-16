open! Core
open! Import
include Nfa_base

let eval_at t input ~offset =
  let rec eval_inner t input ~offset ~current_state =
    if State.equal current_state (accepting_state t)
    then true
    else (
      let edges = node t current_state in
      Node.exists edges ~f:(fun rule target ->
        match rule with
        | None -> eval_inner t input ~offset ~current_state:target
        | Some rule ->
          (match Character_rule.matches rule ~input ~offset with
           | None -> false
           | Some consumed ->
             eval_inner t input ~offset:(offset + consumed) ~current_state:target)) [@nontail
                                                                                      ])
  in
  eval_inner t input ~current_state:(initial_state t) ~offset
;;

let rec eval_from t input ~offset =
  if offset >= Slice.length input
  then false
  else eval_at t input ~offset || eval_from t input ~offset:(offset + 1)
;;

let eval t input =
  let initial_edges = node t (initial_state t) in
  match initial_edges with
  | [: (Some Start_of_line, _) :] -> eval_at t input ~offset:0
  | [: (Some (Literal l), _) :] ->
    (* Fast path: use [Slice.Search_pattern] to find the start point, then use
       NFA matching. *)
    With_return.with_return (fun { return } ->
      Slice.Search_pattern.indexes l input ~f:(fun offset ->
        if eval_from t input ~offset then return true);
      false) [@nontail]
  | _ -> eval_from t input ~offset:0
;;
