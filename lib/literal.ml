open! Core
open! Import

type t =
  { look_for : Slice.Search_pattern.t
  ; flags : Flags.t
  }
[@@deriving sexp_of]

let compile (re : Regex0.t) =
  match re.re with
  | String literal ->
    { look_for = Slice.Search_pattern.create (Slice.of_string literal); flags = re.flags }
  | _ -> failwith "Literal matcher doesn't support non-literals."
;;

let eval t (local_ haystack) =
  let open I64.O in
  if Flags.mem t.flags Require_sol
  then (
    let pattern = Slice.Search_pattern.pattern t.look_for in
    let eol_length_match =
      if Flags.mem t.flags Require_eol
      then Slice.length pattern = Slice.length haystack
      else true
    in
    eol_length_match
    &&
    match Slice.slice haystack ~pos:#0L ~len:(Slice.length pattern) with
    | None -> false
    | Some haystack -> Slice.memcmp pattern haystack [@nontail])
  else if Flags.mem t.flags Require_eol
  then (
    let pattern = Slice.Search_pattern.pattern t.look_for in
    match
      Slice.slice
        haystack
        ~pos:(Slice.length haystack - Slice.length pattern)
        ~len:(Slice.length pattern)
    with
    | None -> false
    | Some haystack -> Slice.memcmp pattern haystack [@nontail])
  else (
    let index = Slice.Search_pattern.index t.look_for haystack in
    I64.Option.is_some index [@nontail])
;;
