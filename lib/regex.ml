open! Core
open! Import

type t =
  | String of string
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
  let rec to_nfa_inner t ~(local_ nfa) ~current_state =
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
    | String s ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Literal (String.Search_pattern.create s)));
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
        (Some (One_of (Iarray.of_list g)));
      accepting_state
    | Neg_group g ->
      let accepting_state = Nfa.Builder.fresh_state nfa in
      Nfa.Builder.add_edge
        nfa
        ~from_state:current_state
        ~to_state:accepting_state
        (Some (Not_one_of (Iarray.of_list g)));
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
        to_nfa_inner t ~nfa ~current_state) [@nontail]
  in
  Nfa.build (fun builder ->
    to_nfa_inner
      t
      ~nfa:builder
      ~current_state:(Nfa.Builder.fresh_state builder) [@nontail])
;;

let parse_escaped parser =
  match Parser.take parser with
  | Some 'w' -> Ok (Class Alphanumeric)
  | Some 'd' -> Ok (Class Numeric)
  | Some '\\' -> Ok (String "\\")
  | Some c -> Or_error.error_string [%string "Unexpected escape sequence \\%{c#Char}."]
  | None -> Or_error.error_string "Unexpected end of pattern. Expected one of [wd\\]."
;;

let parse_group parser =
  let open Or_error.Let_syntax in
  let neg =
    if Option.equal__local Char.equal__local (Parser.peek parser) (Some '^')
    then (
      Parser.drop_exn parser;
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
  let chars = List.rev chars in
  if neg then Ok (Neg_group chars) else Ok (Group chars)
;;

let rev_group_by l ~f =
  let rec aux l ~f ~acc =
    match l with
    | [] -> acc
    | hd :: tl ->
      let acc =
        match f hd with
        | First x ->
          (match acc with
           | First y :: xs -> First (x :: y) :: xs
           | xs -> First [ x ] :: xs)
        | Second x ->
          (match acc with
           | Second y :: xs -> Second (x :: y) :: xs
           | xs -> Second [ x ] :: xs)
      in
      aux tl ~f ~acc
  in
  aux l ~f ~acc:[]
;;

let rec parse_bracketed parser =
  let open Or_error.Let_syntax in
  let%bind seq = parse_sequence parser [] in
  match seq with
  | [ t ] -> Ok t
  | seqs -> Ok (Seq seqs)

and parse_sequence parser acc =
  let open Or_error.Let_syntax in
  let optimise_seq acc =
    rev_group_by acc ~f:(function
      | String s -> First s
      | other -> Second other)
    |> List.concat_map ~f:(function
      | First strings -> [ String (String.concat strings) ]
      | Second others -> others)
  in
  match Parser.take parser with
  | None ->
    (* Attempt to optimise adjacent string literals by concatenating them together. *)
    Ok (optimise_seq acc)
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
    Ok (optimise_seq acc)
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
  | Some c -> parse_sequence parser (String (Char.to_string c) :: acc)
;;

let of_string s =
  let parser = Parser.create s in
  match%bind.Or_error parse_sequence parser [] with
  | [ just ] -> Ok just
  | other -> Ok (Seq other)
;;

module Compiled = struct
  type t = Nfa.t [@@deriving sexp_of]

  let matches t input = Nfa.eval t input
end

let compile t : Compiled.t = to_nfa t
