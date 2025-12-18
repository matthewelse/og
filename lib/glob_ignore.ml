open! Core
open! Import

type t = (#(string * Regex.Compiled.t) Vec.t[@kind value & value])

let create () = (Vec.create [@kind value & value]) ()

let should_ignore t path =
  (Vec.exists [@kind value & value]) t ~f:(fun #(dir, regex) ->
    match String.chop_prefix path ~prefix:dir with
    | None -> false
    | Some path -> Regex.Compiled.matches regex (Slice.of_string path))
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
  (Vec.push_back [@kind value & value]) t #(dir, regex)
;;
