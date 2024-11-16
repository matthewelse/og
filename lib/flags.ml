open! Core

module Flag = struct
  type t =
    | Require_sol
    | Require_eol
  [@@deriving compare, sexp_of]

  include functor Comparable.Make_plain
end

type t = Flag.Set.t [@@deriving compare, sexp_of]

let empty = Flag.Set.empty
let singleton = Flag.Set.singleton
let add = Set.add
let mem = Set.mem
