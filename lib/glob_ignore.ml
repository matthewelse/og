open! Core
open Unboxed
open! Import

type t = (string * Regex.Compiled.t) Vec.t

let create () = Vec.create ()

let should_ignore t path =
  let path = Slice.of_string path in
  Vec.exists t ~f:(fun (dir, regex) ->
    let dir_slice = Slice.of_string dir in
    let open I64.O in
    Slice.length path >= Slice.length dir_slice
    &&
    match Slice.memcmp (Slice.unsafe_slice path ~pos:#0L ~len:(Slice.length dir_slice)) dir_slice with
    | false -> false
    | true ->
      Regex.Compiled.matches
        regex
        (Slice.unsafe_slice
           path
           ~pos:(Slice.length dir_slice)
           ~len:(Slice.length path - Slice.length dir_slice)) [@nontail])
;;

let parse_gitignore (t : t) ~dir ~gitignore_path =
  let rules =
    In_channel.read_lines gitignore_path
    |> List.filter_map ~f:(fun line ->
      if (not (String.is_empty line)) && Char.( <> ) line.[0] '#'
      then (
        match Glob.parse_string line with
        | Ok glob -> Some glob
        | Error _ -> None)
      else None)
  in
  let regex =
    Regex.compile
      { re = Regex.Rule.or_list rules
      ; flags = Flags.of_list [ Require_eol; Require_sol ]
      }
  in
  Vec.push_back t (dir, regex)
;;
