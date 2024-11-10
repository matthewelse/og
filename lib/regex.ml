open! Base
open! Import

type t =
  | Char of char
  | Class of Character_class.t
  | End_of_line
  | Group of char list
  | Neg_group of char list
  | Opt of t
  | Or of t * t
  | Rep1 of t
  | Seq of t list
  | Start_of_line
[@@deriving sexp_of]

let to_nfa t =
  let rec to_nfa_inner t ~nfa ~current_state =
    match t with
    | Start_of_line ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some Start_of_line);
      accepting_state
    | End_of_line ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some End_of_line);
      accepting_state
    | Char c ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (One_of [| c |]));
      accepting_state
    | Class c ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Class c));
      accepting_state
    | Group g ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (One_of (Array.of_list g)));
      accepting_state
    | Neg_group g ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Not_one_of (Array.of_list g)));
      accepting_state
    | Opt t ->
      let accepting_state = to_nfa_inner t ~nfa ~current_state in
      Nfa.Builder.add_edge nfa ~from_state:current_state ~to_state:accepting_state None;
      accepting_state
    | Or (first, second) ->
      let first_accepting_state = to_nfa_inner first ~nfa ~current_state in
      let second_accepting_state = to_nfa_inner second ~nfa ~current_state in
      Nfa.Builder.add_edge
        nfa
        ~from_state:second_accepting_state
        ~to_state:first_accepting_state
        None;
      first_accepting_state
    | Rep1 t ->
      let accepting_state = to_nfa_inner t ~nfa ~current_state in
      Nfa.Builder.add_edge nfa ~from_state:accepting_state ~to_state:current_state None;
      accepting_state
    | Seq ts ->
      List.fold ts ~init:current_state ~f:(fun current_state t ->
        to_nfa_inner t ~nfa ~current_state)
  in
  Nfa.build (fun builder ->
    to_nfa_inner t ~nfa:builder ~current_state:(Nfa.Builder.fresh_state builder))
;;

let parse_escaped parser =
  match Parser.take parser with
  | Some 'w' -> Ok (Class Alphanumeric)
  | Some 'd' -> Ok (Class Numeric)
  | Some '\\' -> Ok (Char '\\')
  | Some c -> Or_error.error_string [%string "Unexpected escape sequence \\%{c#Char}."]
  | None -> Or_error.error_string "Unexpected end of pattern. Expected one of [wd\\]."
;;

let parse_group parser =
  let open Or_error.Let_syntax in
  let neg =
    if [%compare.equal: char option] (Parser.peek parser) (Some '^')
    then (
      Parser.take parser |> (ignore : char option -> unit);
      true)
    else false
  in
  let rec aux acc =
    match Parser.take parser with
    | Some ']' -> Ok acc
    | Some c -> aux (c :: acc)
    | None ->
      Or_error.error_string [%string "Unexpected end of pattern while parsing a group."]
  in
  let%bind chars = aux [] in
  if neg then Ok (Neg_group chars) else Ok (Group chars)
;;

let rec parse_bracketed parser =
  let open Or_error.Let_syntax in
  let%bind seq = parse_sequence parser [] in
  match seq with
  | [ t ] -> Ok t
  | seqs -> Ok (Seq seqs)

and parse_sequence parser acc =
  let open Or_error.Let_syntax in
  match Parser.take parser with
  | None -> Ok (List.rev acc)
  | Some '\\' ->
    let%bind t = parse_escaped parser in
    parse_sequence parser (t :: acc)
  | Some '[' ->
    let%bind group = parse_group parser in
    parse_sequence parser (group :: acc)
  | Some '(' ->
    let%bind t = parse_bracketed parser in
    parse_sequence parser (t :: acc)
  | Some ')' ->
    (* Intentionally stop at this point. *)
    Ok acc
  | Some '|' ->
    let left = Seq acc in
    let%bind right = parse_sequence parser [] in
    parse_sequence parser [ Or (left, Seq right) ]
  | Some '^' -> parse_sequence parser (Start_of_line :: acc)
  | Some '$' -> parse_sequence parser (End_of_line :: acc)
  | Some '+' ->
    (match acc with
     | prev :: acc -> parse_sequence parser (Rep1 prev :: acc)
     | [] -> Or_error.error_string "Saw + operator, but there was nothing before that.")
  | Some '?' ->
    (match acc with
     | prev :: acc -> parse_sequence parser (Opt prev :: acc)
     | [] -> Or_error.error_string "Saw + operator, but there was nothing before that.")
  | Some '.' -> parse_sequence parser (Neg_group [] :: acc)
  | Some c -> parse_sequence parser (Char c :: acc)
;;

let of_string s =
  let parser = Parser.create s in
  match%bind.Or_error parse_sequence parser [] with
  | [ just ] -> Ok just
  | other -> Ok (Seq other)
;;

module Compiled = struct
  type t = { nfa : Nfa.t }

  let matches t input =
    With_return.with_return (fun { return } ->
      for offset = 0 to String.length input - 1 do
        if Nfa.eval t.nfa input ~offset then return true
      done;
      false)
  ;;
end

let compile t : Compiled.t =
  let nfa = to_nfa t in
  { nfa }
;;
