(* From ocaml_simd_sse

   The MIT License

   Copyright (c) 2025 Jane Street Group, LLC <opensource-contacts@janestreet.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Stdlib
include Load_store_intf

(* We can functorize instead of duplicating all the intrinsics because the externals are
   not [@@unboxed]. *)

type void : void

module Raw (T : sig
    (** Must be one of [int8x16,int16x8,int32x4,int64x2,float32x4,float64x2]. *)
    type t : vec128
  end) =
struct
  external aligned_load
    :  nativeint#
    -> T.t
    @@ portable
    = "ocaml_simd_neon_unreachable" "caml_native_pointer_load_vec128_aligned"
  [@@noalloc] [@@builtin]

  external unaligned_load
    :  nativeint#
    -> T.t
    @@ portable
    = "ocaml_simd_neon_unreachable" "caml_native_pointer_load_vec128_unaligned"
  [@@noalloc] [@@builtin]

  external aligned_store
    :  nativeint#
    -> T.t
    -> void
    @@ portable
    = "ocaml_simd_neon_unreachable" "caml_native_pointer_store_vec128_aligned"
  [@@noalloc] [@@builtin]

  external unaligned_store
    :  nativeint#
    -> T.t
    -> void
    @@ portable
    = "ocaml_simd_neon_unreachable" "caml_native_pointer_store_vec128_unaligned"
  [@@noalloc] [@@builtin]

  let[@inline] aligned_store mem t =
    let _ : void = aligned_store mem t in
    ()
  ;;

  let[@inline] unaligned_store mem t =
    let _ : void = unaligned_store mem t in
    ()
  ;;
end

module String (T : sig
    (** Must be one of [int8x16,int16x8,int32x4,int64x2,float32x4,float64x2]. *)
    type t : vec128
  end) =
struct
  external get
    :  (string[@local_opt])
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_string_getu128#"

  external unsafe_get
    :  (string[@local_opt])
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_string_getu128u#"

  module Int32_u = struct
    external get
      :  (string[@local_opt])
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_string_getu128#_indexed_by_int32#"

    external unsafe_get
      :  (string[@local_opt])
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_string_getu128u#_indexed_by_int32#"
  end

  module Int64_u = struct
    external get
      :  (string[@local_opt])
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_string_getu128#_indexed_by_int64#"

    external unsafe_get
      :  (string[@local_opt])
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_string_getu128u#_indexed_by_int64#"
  end

  module Nativeint_u = struct
    external get
      :  (string[@local_opt])
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_string_getu128#_indexed_by_nativeint#"

    external unsafe_get
      :  (string[@local_opt])
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_string_getu128u#_indexed_by_nativeint#"
  end
end

module Bytes (T : sig
    (** Must be one of [int8x16,int16x8,int32x4,int64x2,float32x4,float64x2]. *)
    type t : vec128
  end) =
struct
  external get
    :  (bytes[@local_opt]) @ read
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_bytes_getu128#"

  external unsafe_get
    :  (bytes[@local_opt]) @ read
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_bytes_getu128u#"

  external set
    :  (bytes[@local_opt])
    -> byte:int
    -> T.t
    -> unit
    @@ portable
    = "%caml_bytes_setu128#"

  external unsafe_set
    :  (bytes[@local_opt])
    -> byte:int
    -> T.t
    -> unit
    @@ portable
    = "%caml_bytes_setu128u#"

  module Int32_u = struct
    external get
      :  (bytes[@local_opt]) @ read
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_bytes_getu128#_indexed_by_int32#"

    external unsafe_get
      :  (bytes[@local_opt]) @ read
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_bytes_getu128u#_indexed_by_int32#"

    external set
      :  (bytes[@local_opt])
      -> byte:int32#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bytes_setu128#_indexed_by_int32#"

    external unsafe_set
      :  (bytes[@local_opt])
      -> byte:int32#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bytes_setu128u#_indexed_by_int32#"
  end

  module Int64_u = struct
    external get
      :  (bytes[@local_opt]) @ read
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_bytes_getu128#_indexed_by_int64#"

    external unsafe_get
      :  (bytes[@local_opt]) @ read
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_bytes_getu128u#_indexed_by_int64#"

    external set
      :  (bytes[@local_opt])
      -> byte:int64#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bytes_setu128#_indexed_by_int64#"

    external unsafe_set
      :  (bytes[@local_opt])
      -> byte:int64#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bytes_setu128u#_indexed_by_int64#"
  end

  module Nativeint_u = struct
    external get
      :  (bytes[@local_opt]) @ read
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_bytes_getu128#_indexed_by_nativeint#"

    external unsafe_get
      :  (bytes[@local_opt]) @ read
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_bytes_getu128u#_indexed_by_nativeint#"

    external set
      :  (bytes[@local_opt])
      -> byte:nativeint#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bytes_setu128#_indexed_by_nativeint#"

    external unsafe_set
      :  (bytes[@local_opt])
      -> byte:nativeint#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bytes_setu128u#_indexed_by_nativeint#"
  end
end

module Bigstring (T : sig
    (** Must be one of [int8x16,int16x8,int32x4,int64x2,float32x4,float64x2]. *)
    type t : vec128
  end) =
struct
  type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external unaligned_get
    :  (bigstring[@local_opt]) @ read
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_bigstring_getu128#"

  external unsafe_unaligned_get
    :  (bigstring[@local_opt]) @ read
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_bigstring_getu128u#"

  external aligned_get
    :  (bigstring[@local_opt]) @ read
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_bigstring_geta128#"

  external unsafe_aligned_get
    :  (bigstring[@local_opt]) @ read
    -> byte:int
    -> T.t
    @@ portable
    = "%caml_bigstring_geta128u#"

  external unaligned_set
    :  (bigstring[@local_opt])
    -> byte:int
    -> T.t
    -> unit
    @@ portable
    = "%caml_bigstring_setu128#"

  external unsafe_unaligned_set
    :  (bigstring[@local_opt])
    -> byte:int
    -> T.t
    -> unit
    @@ portable
    = "%caml_bigstring_setu128u#"

  external aligned_set
    :  (bigstring[@local_opt])
    -> byte:int
    -> T.t
    -> unit
    @@ portable
    = "%caml_bigstring_seta128#"

  external unsafe_aligned_set
    :  (bigstring[@local_opt])
    -> byte:int
    -> T.t
    -> unit
    @@ portable
    = "%caml_bigstring_seta128u#"

  module Int32_u = struct
    external unaligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_bigstring_getu128#_indexed_by_int32#"

    external unsafe_unaligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_bigstring_getu128u#_indexed_by_int32#"

    external aligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_bigstring_geta128#_indexed_by_int32#"

    external unsafe_aligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int32#
      -> T.t
      @@ portable
      = "%caml_bigstring_geta128u#_indexed_by_int32#"

    external unaligned_set
      :  (bigstring[@local_opt])
      -> byte:int32#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_setu128#_indexed_by_int32#"

    external unsafe_unaligned_set
      :  (bigstring[@local_opt])
      -> byte:int32#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_setu128u#_indexed_by_int32#"

    external aligned_set
      :  (bigstring[@local_opt])
      -> byte:int32#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_seta128#_indexed_by_int32#"

    external unsafe_aligned_set
      :  (bigstring[@local_opt])
      -> byte:int32#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_seta128u#_indexed_by_int32#"
  end

  module Int64_u = struct
    external unaligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_bigstring_getu128#_indexed_by_int64#"

    external unsafe_unaligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_bigstring_getu128u#_indexed_by_int64#"

    external aligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_bigstring_geta128#_indexed_by_int64#"

    external unsafe_aligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:int64#
      -> T.t
      @@ portable
      = "%caml_bigstring_geta128u#_indexed_by_int64#"

    external unaligned_set
      :  (bigstring[@local_opt])
      -> byte:int64#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_setu128#_indexed_by_int64#"

    external unsafe_unaligned_set
      :  (bigstring[@local_opt])
      -> byte:int64#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_setu128u#_indexed_by_int64#"

    external aligned_set
      :  (bigstring[@local_opt])
      -> byte:int64#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_seta128#_indexed_by_int64#"

    external unsafe_aligned_set
      :  (bigstring[@local_opt])
      -> byte:int64#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_seta128u#_indexed_by_int64#"
  end

  module Nativeint_u = struct
    external unaligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_bigstring_getu128#_indexed_by_nativeint#"

    external unsafe_unaligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_bigstring_getu128u#_indexed_by_nativeint#"

    external aligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_bigstring_geta128#_indexed_by_nativeint#"

    external unsafe_aligned_get
      :  (bigstring[@local_opt]) @ read
      -> byte:nativeint#
      -> T.t
      @@ portable
      = "%caml_bigstring_geta128u#_indexed_by_nativeint#"

    external unaligned_set
      :  (bigstring[@local_opt])
      -> byte:nativeint#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_setu128#_indexed_by_nativeint#"

    external unsafe_unaligned_set
      :  (bigstring[@local_opt])
      -> byte:nativeint#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_setu128u#_indexed_by_nativeint#"

    external aligned_set
      :  (bigstring[@local_opt])
      -> byte:nativeint#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_seta128#_indexed_by_nativeint#"

    external unsafe_aligned_set
      :  (bigstring[@local_opt])
      -> byte:nativeint#
      -> T.t
      -> unit
      @@ portable
      = "%caml_bigstring_seta128u#_indexed_by_nativeint#"
  end
end

module Raw_Int8x16 = Raw (struct
    type nonrec t = int8x16#
  end)

module Raw_Int16x8 = Raw (struct
    type nonrec t = int16x8#
  end)

module Raw_Int32x4 = Raw (struct
    type nonrec t = int32x4#
  end)

module Raw_Int64x2 = Raw (struct
    type nonrec t = int64x2#
  end)

module Raw_Float32x4 = Raw (struct
    type nonrec t = float32x4#
  end)

module Raw_Float64x2 = Raw (struct
    type nonrec t = float64x2#
  end)

module String_Int8x16 = String (struct
    type nonrec t = int8x16#
  end)

module String_Int16x8 = String (struct
    type nonrec t = int16x8#
  end)

module String_Int32x4 = String (struct
    type nonrec t = int32x4#
  end)

module String_Int64x2 = String (struct
    type nonrec t = int64x2#
  end)

module String_Float32x4 = String (struct
    type nonrec t = float32x4#
  end)

module String_Float64x2 = String (struct
    type nonrec t = float64x2#
  end)

module Bytes_Int8x16 = Bytes (struct
    type nonrec t = int8x16#
  end)

module Bytes_Int16x8 = Bytes (struct
    type nonrec t = int16x8#
  end)

module Bytes_Int32x4 = Bytes (struct
    type nonrec t = int32x4#
  end)

module Bytes_Int64x2 = Bytes (struct
    type nonrec t = int64x2#
  end)

module Bytes_Float32x4 = Bytes (struct
    type nonrec t = float32x4#
  end)

module Bytes_Float64x2 = Bytes (struct
    type nonrec t = float64x2#
  end)

module Bigstring_Int8x16 = Bigstring (struct
    type nonrec t = int8x16#
  end)

module Bigstring_Int16x8 = Bigstring (struct
    type nonrec t = int16x8#
  end)

module Bigstring_Int32x4 = Bigstring (struct
    type nonrec t = int32x4#
  end)

module Bigstring_Int64x2 = Bigstring (struct
    type nonrec t = int64x2#
  end)

module Bigstring_Float32x4 = Bigstring (struct
    type nonrec t = float32x4#
  end)

module Bigstring_Float64x2 = Bigstring (struct
    type nonrec t = float64x2#
  end)
