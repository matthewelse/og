open! Core

module type S = sig
  type data

  (** A value of [t] can always be assumed to contain a valid range, i.e. [pos
    >= 0], [len >= 0] and [pos + len <= String.length bytes]. *)
  type t = private
    { global_ bytes : data
    ; pos : int
    ; len : int
    }
  [@@deriving globalize, sexp_of]

  (** Raises if [pos + len > String.length s]. *)
  val create_local : ?pos:int -> ?len:int -> data -> t @ local

  (** Identical behaviour to [create_local], but allocates [t] globally. *)
  val create : ?pos:int -> ?len:int -> data -> t

  val length : t @ local -> int

  (** [at_exn t pos] raises if [pos < 0 || pos >= length t]. *)
  val at_exn : t @ local -> int -> char

  (** [at t pos] returns [None] if [pos < 0 || pos >= length t]. *)
  val at : t @ local -> int -> local_ char option

  (** Returns [None] if [pos + len > length t]. *)
  val slice : t @ local -> pos:int -> len:int -> t option @ local

  (** Raises if [pos + len > length t]. *)
  val slice_exn : t @ local -> pos:int -> len:int -> t @ local

  (** [unsafe_slice t ~pos ~len] is safe if [pos + len <= length t]. *)
  val unsafe_slice : t @ local -> pos:int -> len:int -> t @ local

  (** Returns [true] if the length and contents of the two slices match exactly. *)
  val memcmp : t @ local -> t @ local -> bool

  val memchr : t @ local -> char -> int option @ local
  val to_string : t @ local -> string
  val print_endline : t @ local -> unit

  module Expert : sig
    val bytes : t @ local -> data
    val pos : t @ local -> int
    val len : t @ local -> int
  end

  module Search_pattern : sig
    type slice := t

    type t = private
      { pattern : slice
      ; offsets : int iarray
      }
    [@@deriving sexp_of]

    val create : slice -> t
    val index : t -> slice @ local -> int option @ local
    val indexes : t -> slice @ local -> f:(int -> unit) @ local -> unit
    val pattern : t @ local -> slice @ local
  end
end

module type Slice = sig
  include S with type data := string

  module Bytes : sig
    include S with type data := bytes

    val blit : (t, t) Blit.blit
    val unsafe_blit : (t, t) Blit.blit
  end
end
