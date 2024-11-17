open! Core

module Flag = struct
  type t =
    | Require_sol
    | Require_eol
  [@@deriving compare, enumerate, sexp_of]

  include functor Comparable.Make_plain

  let to_int = function
    | Require_sol -> 0
    | Require_eol -> 1
  ;;
end

type t = int [@@deriving compare]

let flag_bit flag = 1 lsl Flag.to_int flag
let empty = 0
let add t flag = t lor flag_bit flag
let singleton flag = add empty flag
let mem t flag = t land flag_bit flag <> 0

let to_list t =
  List.filter_map Flag.all ~f:(fun flag -> if mem t flag then Some flag else None)
;;

let sexp_of_t t = to_list t |> [%sexp_of: Flag.t list]
