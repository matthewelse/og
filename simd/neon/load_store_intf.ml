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

module type Raw = sig @@ portable
  type t : vec128

  (** Load 16 bytes from an arbitrary address encoded as a [nativeint#]. *)
  val unaligned_load : nativeint# -> t

  (** Store 16 bytes to an arbitrary address encoded as a [nativeint#]. *)
  val unaligned_store : nativeint# -> t -> unit

  (** Load 16 bytes from a 16-byte-aligned address encoded as a [nativeint#]. Does not
      validate alignment. *)
  val aligned_load : nativeint# -> t

  (** Store 16 bytes to a 16-byte-aligned address encoded as a [nativeint#]. Does not
      validate alignment. *)
  val aligned_store : nativeint# -> t -> unit
end

module type String_accessors = sig @@ portable
  type t : vec128
  type index : any

  (** Load 16 bytes from a [string] at an arbitrary byte offset.

      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val get : string @ local -> byte:index -> t

  (** Load 16 bytes from a [string] at an arbitrary byte offset. Does not check bounds. *)
  val unsafe_get : string @ local -> byte:index -> t
end

module type Bytes_accessors = sig @@ portable
  type t : vec128
  type index : any

  (** Load 16 bytes from a [bytes] at an arbitrary byte offset.

      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val get : bytes @ local read -> byte:index -> t

  (** Load 16 bytes from a [bytes] at an arbitrary byte offset. Does not check bounds. *)
  val unsafe_get : bytes @ local read -> byte:index -> t

  (** Write 16 bytes to a [bytes] at an arbitrary byte offset.

      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val set : bytes @ local -> byte:index -> t -> unit

  (** Write 16 bytes to a [bytes] at an arbitrary byte offset. Does not check bounds. *)
  val unsafe_set : bytes @ local -> byte:index -> t -> unit
end

module type Bigstring_accessors = sig @@ portable
  type bigstring :=
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t : vec128
  type index : any

  (** By default, the backing array of a bigstring is allocated by `malloc`, so is always
      16-byte aligned. Therefore, it is valid to use aligned-load/store instructions at
      16-byte intervals in the bigstring. *)

  (** Load 16 bytes from a [bigstring] at an arbitrary byte offset.

      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val unaligned_get : bigstring @ local read -> byte:index -> t

  (** Load 16 bytes from a [bigstring] at a 16-aligned byte offset.

      @raise Invalid_argument if the computed address is not 16-byte aligned.
      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val aligned_get : bigstring @ local read -> byte:index -> t

  (** Load 16 bytes from a [bigstring] at an arbitrary byte offset. Does not check bounds. *)
  val unsafe_unaligned_get : bigstring @ local read -> byte:index -> t

  (** Load 16 bytes from a [bigstring] at a 16-aligned byte offset. Does not check bounds
      or alignment. *)
  val unsafe_aligned_get : bigstring @ local read -> byte:index -> t

  (** Write 16 bytes to a [bigstring] at an arbitrary byte offset.

      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val unaligned_set : bigstring @ local -> byte:index -> t -> unit

  (** Write 16 bytes to a [bigstring] at a 16-aligned byte offset.

      @raise Invalid_argument if the computed address is not 16-byte aligned.
      @raise Invalid_argument if [byte..byte+16] fails bounds checking. *)
  val aligned_set : bigstring @ local -> byte:index -> t -> unit

  (** Write 16 bytes to a [bigstring] at an arbitrary byte offset. Does not check bounds. *)
  val unsafe_unaligned_set : bigstring @ local -> byte:index -> t -> unit

  (** Write 16 bytes to a [bigstring] at a 16-aligned byte offset. Does not check bounds
      or alignment. *)
  val unsafe_aligned_set : bigstring @ local -> byte:index -> t -> unit
end

module type String = sig @@ portable
  (** [t] is one of the 16-byte vector types. *)
  type t : vec128

  (** @inline *)
  include String_accessors with type t := t and type index := int

  module Int32_u : String_accessors with type t := t and type index := int32#
  module Int64_u : String_accessors with type t := t and type index := int64#
  module Nativeint_u : String_accessors with type t := t and type index := nativeint#
end

module type Bytes = sig @@ portable
  (** [t] is one of the 16-byte vector types. *)
  type t : vec128

  (** @inline *)
  include Bytes_accessors with type t := t and type index := int

  module Int32_u : Bytes_accessors with type t := t and type index := int32#
  module Int64_u : Bytes_accessors with type t := t and type index := int64#
  module Nativeint_u : Bytes_accessors with type t := t and type index := nativeint#
end

module type Bigstring = sig @@ portable
  (** [t] is one of the 16-byte vector types. *)
  type t : vec128

  (** @inline *)
  include Bigstring_accessors with type t := t and type index := int

  module Int32_u : Bigstring_accessors with type t := t and type index := int32#
  module Int64_u : Bigstring_accessors with type t := t and type index := int64#
  module Nativeint_u : Bigstring_accessors with type t := t and type index := nativeint#
end

module type Load_store = sig @@ portable
  module type String = String
  module type Bytes = Bytes
  module type Bigstring = Bigstring
  module type String_accessors = String_accessors
  module type Bytes_accessors = Bytes_accessors
  module type Bigstring_accessors = Bigstring_accessors

  module Raw_Int8x16 : Raw with type t := int8x16#
  module Raw_Int16x8 : Raw with type t := int16x8#
  module Raw_Int32x4 : Raw with type t := int32x4#
  module Raw_Int64x2 : Raw with type t := int64x2#
  module Raw_Float32x4 : Raw with type t := float32x4#
  module Raw_Float64x2 : Raw with type t := float64x2#
  module String_Int8x16 : String with type t := int8x16#
  module String_Int16x8 : String with type t := int16x8#
  module String_Int32x4 : String with type t := int32x4#
  module String_Int64x2 : String with type t := int64x2#
  module String_Float32x4 : String with type t := float32x4#
  module String_Float64x2 : String with type t := float64x2#
  module Bytes_Int8x16 : Bytes with type t := int8x16#
  module Bytes_Int16x8 : Bytes with type t := int16x8#
  module Bytes_Int32x4 : Bytes with type t := int32x4#
  module Bytes_Int64x2 : Bytes with type t := int64x2#
  module Bytes_Float32x4 : Bytes with type t := float32x4#
  module Bytes_Float64x2 : Bytes with type t := float64x2#
  module Bigstring_Int8x16 : Bigstring with type t := int8x16#
  module Bigstring_Int16x8 : Bigstring with type t := int16x8#
  module Bigstring_Int32x4 : Bigstring with type t := int32x4#
  module Bigstring_Int64x2 : Bigstring with type t := int64x2#
  module Bigstring_Float32x4 : Bigstring with type t := float32x4#
  module Bigstring_Float64x2 : Bigstring with type t := float64x2#
end
