open! Core

type t = int64# [@@deriving sexp_of]

val to_string : t -> string
external to_int64 : t -> (int64[@local_opt]) = "%box_int64"
external of_int64 : (int64[@local_opt]) -> t = "%unbox_int64"
val splat : char -> t
val ctz : t -> int
val clz : t -> int
val ctz_nonzero : t -> int
val clz_nonzero : t -> int
val min : t -> t -> t
val max : t -> t -> t
val of_int : int -> t
val to_int_trunc : t -> int
val to_int_exn : t -> int

module Ref : sig
  type i64 := t
  type t = private { mutable value : i64 }

  val create_local : i64 -> t @ local
  val create : i64 -> t
  val add : t @ local -> i64 -> unit
  val get : t @ local -> i64
  val set : t @ local -> i64 -> unit
  val incr : t @ local -> unit
  val decr : t @ local -> unit
end

module Option : sig
  type i64 := t

  type t =
    | None
    | Some of i64
  [@@deriving sexp_of]

  val box : t @ local -> int64 option
  val unbox : int64 option -> t @ local
  val is_some : t @ local -> bool
  val value : t @ local -> default:i64 -> i64
end

module Iarray : sig
  external get
    : ('a : any).
    'a iarray @ local -> int64# -> 'a
    = "%array_safe_get_indexed_by_int64#"
  [@@layout_poly] [@@noalloc]

  external unsafe_get
    : ('a : any).
    'a iarray @ local -> int64# -> 'a
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly] [@@noalloc]
end

module Array : sig
  external get
    : ('a : any).
    'a array @ local -> int64# -> 'a
    = "%array_safe_get_indexed_by_int64#"
  [@@layout_poly] [@@noalloc]

  external unsafe_get
    : ('a : any).
    'a array @ local -> int64# -> 'a
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly] [@@noalloc]

  external set
    : ('a : any).
    'a array @ local -> int64# -> 'a -> unit
    = "%array_safe_set_indexed_by_int64#"
  [@@layout_poly] [@@noalloc]

  external unsafe_set
    : ('a : any).
    'a array @ local -> int64# -> 'a -> unit
    = "%array_unsafe_set_indexed_by_int64#"
  [@@layout_poly] [@@noalloc]
end

module String : sig
  val length : string -> t
end

module Bytes : sig
  val unsafe_get : Bytes.t @ local -> t -> char
end

module Bigstring : sig
  val unsafe_get : Bigstring.t @ local -> t -> char
end

module O : sig
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
  val ( lxor ) : t -> t -> t
  val ( lsl ) : t -> int -> t
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val lnot : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ! ) : Ref.t @ local -> t
  val incr : Ref.t @ local -> unit
  val decr : Ref.t @ local -> unit
  val max : t -> t -> t
  val min : t -> t -> t
end

module Hex : sig
  type nonrec t = t [@@deriving sexp_of]
end
