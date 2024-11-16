open! Core

module type S = sig
  type t [@@deriving sexp_of]

  val compile : Regex0.t -> t
  val eval : t -> Slice.t @ local -> bool
end

type t =
  | Nfa_backtrack
  | Nfa_hybrid
[@@deriving enumerate, sexp_of]
