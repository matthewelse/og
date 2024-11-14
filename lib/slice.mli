open! Core

(** A value of [t] can always be assumed to contain a valid range, i.e. [pos >=
  0], [len >= 0] and [pos + len <= String.length bytes]. *)
type t = private
  { global_ bytes : string
  ; pos : int
  ; len : int
  }
[@@deriving sexp_of]

(** Raises if [pos + len > String.length s]. *)
val create_local : ?pos:int -> ?len:int -> string -> t @ local

(** Identical behaviour to [create_local], but allocates [t] globally. *)
val create : ?pos:int -> ?len:int -> string -> t

val length : t @ local -> int

(** [at_exn t pos] raises if [pos < 0 || pos >= length t]. *)
val at_exn : t @ local -> int -> char

(** [at t pos] returns [None] if [pos < 0 || pos >= length t]. *)
val at : t @ local -> int -> local_ char option

(** Raises if [pos + len > length t]. *)
val slice : t @ local -> pos:int -> len:int -> t option @ local

(** [unsafe_slice t ~pos ~len] is safe if [pos + len <= length t]. *)
val unsafe_slice : t @ local -> pos:int -> len:int -> t @ local

(** Returns [true] if the length and contents of the two slices match exactly. *)
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
