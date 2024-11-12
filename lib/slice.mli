open! Core

type t = private
  { global_ bytes : string
  ; pos : int
  ; len : int
  }
[@@deriving sexp_of]

val create_local : ?pos:int -> ?len:int -> string -> t @ local
val create : ?pos:int -> ?len:int -> string -> t
val length : t @ local -> int
val at : t @ local -> int -> char
val slice : ?pos:int -> ?len:int -> t @ local -> t @ local
val memcmp : t @ local -> t @ local -> bool
val to_string : t @ local -> string

module Search_pattern : sig
  type slice := t

  type t = private
    { pattern : slice
    ; offsets : int iarray
    }
  [@@deriving sexp_of]

  val create : slice -> t
  val index : t -> slice @ local -> int option @ local
  val pattern : t @ local -> slice @ local
end
