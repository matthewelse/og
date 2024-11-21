open! Core
open! Import
include Regex0

let parse_escaped parser =
  let open Rule in
  match Parser.take parser with
  | Some 'w' -> Ok (Class Alphanumeric)
  | Some 'd' -> Ok (Class Numeric)
  | Some '\\' -> Ok (String "\\")
  | Some c -> Or_error.error_string [%string "Unexpected escape sequence \\%{c#Char}."]
  | None -> Or_error.error_string "Unexpected end of pattern. Expected one of [wd\\]."
;;

let parse_group parser =
  let open Or_error.Let_syntax in
  let open Rule in
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
  let open Rule in
  let%bind seq = parse_sequence parser [] in
  match seq with
  | [ t ] -> Ok t
  | seqs -> Ok (Seq seqs)

and parse_sequence parser acc =
  let open Or_error.Let_syntax in
  let open Rule in
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
  let flags, s =
    match String.chop_suffix s ~suffix:"$" with
    | None -> Flags.empty, s
    | Some s -> Flags.singleton Require_eol, s
  in
  let parser = Parser.create s in
  let flags =
    if Option.equal__local Char.equal__local (Parser.peek parser) (Some '^')
    then (
      Parser.drop_exn parser;
      Flags.add flags Require_sol)
    else flags
  in
  let%bind.Or_error re =
    match%bind.Or_error parse_sequence parser [] with
    | [ just ] -> Ok just
    | other -> Ok (Rule.Seq other)
  in
  Ok { re; flags }
;;

module Compiled = struct
  type t =
    | T :
        { impl : (module Implementation.S with type t = 't)
        ; compiled : 't
        }
        -> t

  let sexp_of_t (T { impl = (module Impl); compiled }) = Impl.sexp_of_t compiled
  let matches (T { impl = (module Impl); compiled }) input = Impl.eval compiled input
end

let compile ?(impl : Implementation.t option) t : Compiled.t =
  let (module Impl : Implementation.S) =
    match impl with
    | None ->
      (* If you're just matching on literals, use the literal matcher, which
         just uses memcmp/Boyer-Moore, but it can't support much else. *)
      (match t.re with
       | String _ -> (module Literal)
       | _ -> (module Nfa_hybrid))
    | Some Nfa_backtrack -> (module Nfa)
    | Some Nfa_hybrid -> (module Nfa_hybrid)
  in
  let compiled = Impl.compile t in
  T { impl = (module Impl); compiled }
;;
