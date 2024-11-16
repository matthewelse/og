open! Core
open! Import

module Rule = struct
  type t =
    | String of string
    | Class of Character_class.t
    | Group of char list
    | Neg_group of char list
    | Opt of t
    | Or of t * t
    | Rep1 of t
    | Seq of t list
  [@@deriving sexp_of]
end

type t =
  { re : Rule.t
  ; flags : Flags.t
  }
[@@deriving sexp_of]

let best_constant (t : t) =
  let rec aux (re : Rule.t) =
    match re with
    | Class _ | Group _ | Neg_group _ | Opt _ | Or _ | Rep1 _ -> None
    | String s -> Some s
    | Seq ts ->
      let choose_best best_so_far acc =
        match best_so_far with
        | None -> acc
        | Some best_so_far ->
          if String.length best_so_far > String.length acc then best_so_far else acc
      in
      let best_so_far, acc =
        List.fold ts ~init:(None, "") ~f:(fun (best_so_far, acc) re ->
          match aux re with
          | None -> Some (choose_best best_so_far acc), ""
          | Some s' -> best_so_far, acc ^ s')
      in
      if String.length (choose_best best_so_far acc) = 0 then None else Some acc
  in
  aux t.re
;;
