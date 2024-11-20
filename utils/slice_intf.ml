open! Core

module type Search = sig
  type slice
  type t [@@deriving sexp_of]

  val create : slice -> t
  val index : t -> slice @ local -> I64.Option.t @ local
  val indexes : t -> slice @ local -> f:(I64.t -> unit) @ local -> unit
  val pattern : t @ local -> slice @ local
end

module type S = sig
  type data

  (** A value of [t] can always be assumed to contain a valid range, i.e. [pos
    >= 0], [len >= 0] and [pos + len <= String.length bytes]. *)
  type t = private
    { global_ bytes : data
    ; pos : int64#
    ; len : int64#
    }
  [@@deriving globalize, sexp_of]

  (** Raises if [pos + len > String.length s]. *)
  val create_local : data -> pos:int64# -> len:int64# -> t @ local

  (** Identical behaviour to [create_local], but allocates [t] globally. *)
  val create : data -> pos:int64# -> len:int64# -> t

  (** Like [create], but the user is responsible for maintaining the invariants
      enforced in [check_bounds]. *)
  val unsafe_create : data -> pos:int64# -> len:int64# -> t

  val unsafe_create_local : data -> pos:int64# -> len:int64# -> t @ local
  val length : t @ local -> int64#

  (** [at_exn t pos] raises if [pos < 0 || pos >= length t]. *)
  val at_exn : t @ local -> int64# -> char

  (** [at t pos] returns [None] if [pos < 0 || pos >= length t]. *)
  val at : t @ local -> int64# -> local_ char option

  (** Returns [None] if [pos + len > length t]. *)
  val slice : t @ local -> pos:int64# -> len:int64# -> t option @ local

  (** Raises if [pos + len > length t]. *)
  val slice_exn : t @ local -> pos:int64# -> len:int64# -> t @ local

  (** [unsafe_slice t ~pos ~len] is safe if [pos + len <= length t]. *)
  val unsafe_slice : t @ local -> pos:int64# -> len:int64# -> t @ local

  (** Returns [true] if the length and contents of the two slices match exactly. *)
  val memcmp : t @ local -> t @ local -> bool

  val memchr : t @ local -> char -> I64.Option.t @ local
  val to_string : t @ local -> string
  val print_endline : t @ local -> unit
  val iter : t @ local -> f:(char -> unit) @ local -> unit

  module Expert : sig
    val bytes : t @ local -> data
    val pos : t @ local -> int64#
    val len : t @ local -> int64#
  end

  module BMH : Search with type slice := t
  module Boyer_moore : Search with type slice := t
  module KMP : Search with type slice := t
  module Search_pattern : Search with type slice := t
end

module type Slice = sig
  include S with type data := string

  val of_string : string -> t

  module Bytes : sig
    include S with type data := bytes

    val blit : (t, t) Blit.blit
    val unsafe_blit : (t, t) Blit.blit
  end
end
