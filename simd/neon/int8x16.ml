open! Base
open! Unboxed

type t = int8x16#
type mask = int8x16#

external const
  :  int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_int8x16_const16"
[@@noalloc] [@@builtin]

external const1
  :  int8#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_int8x16_const1"
[@@noalloc] [@@builtin]

external set1
  :  i64
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_dup_n"
[@@noalloc] [@@builtin]

external ( = )
  :  t
  -> t
  -> mask
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_cmpeq"
[@@noalloc] [@@builtin]

external ( lor )
  :  t
  -> t
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_bitwise_or"
[@@noalloc] [@@builtin]

external extract
  :  (int[@untagged])
  -> t
  -> int8#
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_extract"
[@@noalloc] [@@builtin]

external of_int8x16_bits
  :  int8x16#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external of_int16x8_bits
  :  int16x8#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external of_int32x4_bits
  :  int32x4#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external of_int64x2_bits
  :  int64x2#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external maxv_raw
  :  t
  -> float#
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_maxv"
[@@builtin] [@@noalloc]

external minv_raw
  :  t
  -> float#
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_minv"
[@@builtin] [@@noalloc]

let maxv t = maxv_raw t |> F64.to_bits
let minv t = minv_raw t |> F64.to_bits

module String = Load_store.String_Int8x16
module Bytes = Load_store.Bytes_Int8x16
module Bigstring = Load_store.Bigstring_Int8x16
module Raw = Load_store.Raw_Int8x16

let to_tuple t =
  let n0 = extract 0 t in
  let n1 = extract 1 t in
  let n2 = extract 2 t in
  let n3 = extract 3 t in
  let n4 = extract 4 t in
  let n5 = extract 5 t in
  let n6 = extract 6 t in
  let n7 = extract 7 t in
  let n8 = extract 8 t in
  let n9 = extract 9 t in
  let n10 = extract 10 t in
  let n11 = extract 11 t in
  let n12 = extract 12 t in
  let n13 = extract 13 t in
  let n14 = extract 14 t in
  let n15 = extract 15 t in
  #(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)
;;

external i8_to_i64 : int8# -> int64# @@ portable = "%int64#_of_int8#"

let sexp_of_t t =
  let #(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15) =
    to_tuple t
  in
  let i64 n =
    let open I64.O in
    i8_to_i64 n land #0xFFL
  in
  let #(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15) =
    #( i64 n0
     , i64 n1
     , i64 n2
     , i64 n3
     , i64 n4
     , i64 n5
     , i64 n6
     , i64 n7
     , i64 n8
     , i64 n9
     , i64 n10
     , i64 n11
     , i64 n12
     , i64 n13
     , i64 n14
     , i64 n15 )
  in
  [%sexp
    (#(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)
     : #(I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t
        * I64.Hex_unsigned.t))]
;;
