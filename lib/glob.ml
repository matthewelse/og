open! Core
open! Import

(* https://git-scm.com/docs/gitignore *)

let parse_string line : (Regex0.Rule.t, int * string) result =
  let len = String.length line in
  if len = 0 || Char.equal line.[0] '#'
  then Ok (Regex0.Rule.Seq [])
  else (
    let is_negated =
      match line.[0] with
      | '!' ->
        (* TODO: make this actually work *)
        true
      | _ -> false
    in
    let partial_string = Vec.create () in
    let parts = Vec.create () in
    let maybe_add_string () =
      if not (Vec.is_empty partial_string)
      then (
        let str =
          String.init (Vec.length partial_string) ~f:(fun i ->
            Vec.unsafe_get partial_string i)
        in
        Vec.clear partial_string;
        Vec.push_back parts (`String str))
      else ()
    in
    let rec aux line ~pos ~len =
      if pos >= len
      then ()
      else (
        match line.[pos] with
        | '\\' ->
          if pos + 1 >= len
          then failwith "invalid"
          else (
            Vec.push_back partial_string line.[pos + 1];
            aux line ~pos:(pos + 2) ~len)
        | '?' ->
          maybe_add_string ();
          Vec.push_back parts `Question;
          aux line ~pos:(pos + 1) ~len
        | '*' ->
          maybe_add_string ();
          if pos + 1 < len && Char.equal line.[pos + 1] '*'
          then (
            Vec.push_back parts `Star_star;
            aux line ~pos:(pos + 2) ~len)
          else (
            Vec.push_back parts `Star;
            aux line ~pos:(pos + 1) ~len)
        | c ->
          Vec.push_back partial_string c;
          aux line ~pos:(pos + 1) ~len)
    in
    aux line ~pos:(if is_negated then 1 else 0) ~len;
    maybe_add_string ();
    let convert_part : _ -> Regex0.Rule.t = function
      | `String s -> String s
      | `Question -> Neg_group [ '/' ]
      | `Star -> Rep1 (Neg_group [ '/' ])
      | `Star_star -> Rep1 (Neg_group [])
    in
    let regex : Regex0.Rule.t =
      if Vec.length parts = 1
      then convert_part (Vec.get parts 0)
      else Seq (Vec.to_list parts |> List.map ~f:convert_part)
    in
    (* print_s [%sexp (parts : [ `String of string | `Question | `Star | `Star_star ] Vec.t)];
    *)
    Ok regex)
;;
