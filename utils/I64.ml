open! Core
include Stdlib_upstream_compatible.Int64_u

type t = int64#

let sexp_of_t t = Int64.sexp_of_t (to_int64 t)

(* FIXME: This use of intrinsics might be broken in amd64, which needs special
handling for n=0. *)

external ctz
  :  int64#
  -> (int[@untagged])
  = "caml_int64_ctz" "caml_int64_ctz_unboxed_to_untagged"
[@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external clz
  :  int64#
  -> (int[@untagged])
  = "caml_int64_clz" "caml_int64_clz_unboxed_to_untagged"
[@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

let[@inline always] to_int_trunc t = to_int t
let[@inline always] to_int_exn t = Int64.to_int_exn (to_int64 t)
let to_string t = Int64.to_string (to_int64 t)

module Ref = struct
  type i64 = t
  type t = { mutable value : i64 }

  let[@inline always] create_local n = exclave_ { value = n }
  let[@inline always] create n = { value = n }
  let[@inline always] add t n = t.value <- add t.value n
  let[@inline always] get t = t.value
  let[@inline always] set t n = t.value <- n
  let[@inline always] incr t = add t #1L
  let[@inline always] decr t = add t (-#1L)
end

module Option = struct
  type i64 = t [@@deriving sexp_of]

  type t =
    | None
    | Some of i64
  [@@deriving sexp_of]

  let box : t @ local -> _ option = function
    | None -> None
    | Some n -> Some (to_int64 n)
  ;;

  let unbox : _ option -> t @ local = function
    | None -> None
    | Some n -> Some (of_int64 n)
  ;;

  let is_some = function
    | None -> false
    | Some _ -> true
  ;;

  let value (local_ t) ~default =
    match t with
    | None -> default
    | Some n -> n
  ;;
end

module Iarray = struct
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

module Array = struct
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

module String = struct
  let[@inline always] length t = String.length t |> of_int
end

module Bytes = struct
  let[@inline always] unsafe_get t n = Bytes.unsafe_get t (to_int_trunc n)
end

module Bigstring = struct
  let[@inline always] unsafe_get t n = Bigstring.unsafe_get t (to_int_trunc n)
end

module O = struct
  let ( land ) = logand
  let ( lor ) = logor
  let ( lxor ) = logxor
  let ( lsl ) = shift_left
  let ( = ) = equal
  let[@inlne always] ( <> ) t t' = Int64.O.( <> ) (to_int64 t) (to_int64 t')
  let[@inlne always] ( > ) t t' = Int64.O.( > ) (to_int64 t) (to_int64 t')
  let[@inlne always] ( < ) t t' = Int64.O.( < ) (to_int64 t) (to_int64 t')
  let[@inlne always] ( <= ) t t' = Int64.O.( <= ) (to_int64 t) (to_int64 t')
  let[@inlne always] ( >= ) t t' = Int64.O.( >= ) (to_int64 t) (to_int64 t')
  let lnot = lognot
  let ( * ) = mul
  let ( / ) = div
  let ( + ) = add
  let ( - ) = sub
  let ( ! ) = Ref.get
  let incr = Ref.incr
  let decr = Ref.decr
  let min = min
  let max = max
end

include O

let[@inline always] splat c =
  let c = Char.to_int c |> of_int in
  let open O in
  c
  lor (c lsl 8)
  lor (c lsl 16)
  lor (c lsl 24)
  lor (c lsl 32)
  lor (c lsl 40)
  lor (c lsl 48)
  lor (c lsl 56)
;;

module Hex = struct
  type nonrec t = t

  let sexp_of_t t = Int64.Hex.sexp_of_t (to_int64 t)
end
