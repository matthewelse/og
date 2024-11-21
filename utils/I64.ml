open! Core
include Stdlib_upstream_compatible.Int64_u

type t = int64#

let sexp_of_t t = Int64.sexp_of_t (to_int64 t)
let[@inline always] ctz t = Int64.ctz (to_int64 t)
let[@inline always] clz t = Int64.clz (to_int64 t)
let to_int_trunc t = to_int t

module Ref = struct
  type i64 = t
  type t = { mutable value : i64 }

  let create_local n = exclave_ { value = n }
  let create n = { value = n }
  let add t n = t.value <- add t.value n
  let get t = t.value
  let set t n = t.value <- n
  let incr t = add t #1L
  let decr t = add t (-#1L)
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
  let length t = String.length t |> of_int
end

module Bytes = struct
  let unsafe_get t n = Bytes.unsafe_get t (to_int_trunc n)
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

let splat c =
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
