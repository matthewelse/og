open! Core
open Unboxed
open! Import

type t = (#(Slice.t * Regex.Compiled.t) Vec.t[@kind value & value])

let create () = (Vec.create [@kind value & value]) ()

let should_ignore t path =
  let path = Slice.of_string path in
  (Vec.exists [@kind value & value]) t ~f:(fun #(dir, regex) ->
    let open I64.O in
    Slice.length path >= Slice.length dir
    &&
    match Slice.memcmp path dir with
    | false -> false
    | true ->
      Regex.Compiled.matches
        regex
        (Slice.unsafe_slice
           path
           ~pos:(Slice.length dir)
           ~len:(Slice.length path - Slice.length dir)) [@nontail])
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
  (Vec.push_back [@kind value & value]) t #(Slice.of_string dir, regex)
;;
