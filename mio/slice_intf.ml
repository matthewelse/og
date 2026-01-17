open! Core
open Unboxed

module type Search = sig @@ portable
  type slice : any
  type t [@@deriving sexp_of]

  val create : slice -> t
  val index : t -> slice -> I64.Option.t @ local
  val indexes : t -> slice -> f:(I64.t -> unit) @ local -> unit
  val pattern : t -> slice
end

module type S = sig @@ portable
  type data

  (** A value of [t] can always be assumed to contain a valid range, i.e. [pos >= 0],
      [len >= 0] and [pos + len <= String.length bytes]. *)
  type t = private #{ global_ bytes : data; pos : int64#; len : int64# }
  [@@deriving sexp_of]

  val to_string : t -> string
  val of_string : string -> t

  (** Unboxed optional slice using len = #-1L as sentinel for None.
      Use [match%optional_u.Slice.Option] for pattern matching. *)
  module Option : sig @@ portable
    type outer := t

    (** Abstract type to distinguish optional slices from regular slices.
        Has the same representation as [Slice.t] but prevents accidental misuse. *)
    type t : value & bits64 & bits64

    val none : unit -> t
    val some : outer -> t
    val is_none : t -> bool
    val is_some : t -> bool
    val unsafe_value : t -> outer
    val value_exn : t -> outer
    val value : t -> default:outer -> outer

    module Optional_syntax : sig @@ portable
      module Optional_syntax : sig @@ portable
        val is_none : t -> bool
        val unsafe_value : t -> outer
      end
    end
  end

  (** Raises if [pos + len > String.length s]. *)
  val create : data -> pos:int64# -> len:int64# -> t

  (** Alias for [create] (no distinction needed with unboxed records). *)
  val create_local : data -> pos:int64# -> len:int64# -> t

  (** Like [create], but the user is responsible for maintaining the invariants enforced
      in [check_bounds]. *)
  val unsafe_create : data -> pos:int64# -> len:int64# -> t

  (** Alias for [unsafe_create]. *)
  val unsafe_create_local : data -> pos:int64# -> len:int64# -> t
  val length : t -> int64#

  (** [get_exn t pos] raises if [pos < 0 || pos >= length t]. *)
  val get_exn : t -> int64# -> char

  (** [get t pos] returns [None] if [pos < 0 || pos >= length t]. *)
  val get : t -> int64# -> char or_null

  val unsafe_get : t -> int64# -> char

  (** Returns [Option.none ()] if [pos + len > length t]. *)
  val slice : t -> pos:int64# -> len:int64# -> Option.t

  (** Raises if [pos + len > length t]. *)
  val slice_exn : t -> pos:int64# -> len:int64# -> t

  val slice_from : t -> pos:int64# -> Option.t

  (** [unsafe_slice t ~pos ~len] is safe if [pos + len <= length t]. *)
  val unsafe_slice : t -> pos:int64# -> len:int64# -> t

  (** Returns [true] if the length and contents of the two slices match exactly. *)
  val memcmp : t -> t -> bool

  (** [memchr t x] returns the first index of [c] in [t], if it exists. Returns [None]
      otherwise. *)
  val memchr : t -> char -> I64.Option.t @ local

  (** [rmemchr t c] returns the _last_ index of [c] in [t], if it exists. Otherwise, it
      returns [None]. *)
  val rmemchr : t -> char -> I64.Option.t @ local

  val print_endline : t -> unit
  val iter : t -> f:(char -> unit) @ local -> unit
  val exists : t -> f:(char -> bool) @ local -> bool

  module Expert : sig @@ portable
    val bytes : t -> data
    val pos : t -> int64#
    val len : t -> int64#
  end

  module BMH : Search with type slice := t
  module Boyer_moore : Search with type slice := t
  module KMP : Search with type slice := t
  module Search_pattern : Search with type slice := t
end

module type S_mutable = sig @@ portable
  include S

  val blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  val unsafe_blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
end

module type Slice = sig @@ portable
  include S_mutable with type data := Bigstring.t
end
