open! Base
open! Unboxed

module%template
  [@kind k = (word & word & word, value & bits64 & bits64)] Make
    (Vec : sig
       type t : vec128 [@@deriving sexp_of]

       val ( = ) : t -> t -> t
       val ( lor ) : t -> t -> t
       val movemask : t -> i64
       val minv : t -> i64
       val set1 : i64 -> t
       val size_bytes : unit -> i64
     end)
    (String : sig
       type t

       module Ptr : sig
         type str := t
         type t : k

         val unsafe_get_vec : t -> Vec.t
         val unsafe_get_char : t -> char
         val advance : t -> i64 -> t
         val in_range : t -> bool
         val finalize : t -> str -> I64.Option.t
       end

       val root : t -> pos:i64 -> len:i64 -> Ptr.t
     end) =
struct
  module Unrolled_vec = struct
    type t = #(Vec.t * Vec.t * Vec.t * Vec.t) [@@deriving sexp_of]

    let vector_size () = #64L

    let load s : t =
      let open I64.O in
      let chunk0 = String.Ptr.unsafe_get_vec s in
      let chunk1 = String.Ptr.unsafe_get_vec (String.Ptr.advance s (Vec.size_bytes ())) in
      let chunk2 =
        String.Ptr.unsafe_get_vec (String.Ptr.advance s (Vec.size_bytes () * #2L))
      in
      let chunk3 =
        String.Ptr.unsafe_get_vec (String.Ptr.advance s (Vec.size_bytes () * #3L))
      in
      #(chunk0, chunk1, chunk2, chunk3)
    ;;

    let compare #(chunk0, chunk1, chunk2, chunk3) mask : t =
      let mask0 = Vec.(chunk0 = mask) in
      let mask1 = Vec.(chunk1 = mask) in
      let mask2 = Vec.(chunk2 = mask) in
      let mask3 = Vec.(chunk3 = mask) in
      #(mask0, mask1, mask2, mask3)
    ;;

    let ( lor ) #(chunk0, chunk1, chunk2, chunk3) #(chunk0', chunk1', chunk2', chunk3') =
      let open Vec in
      let chunk0 = chunk0 lor chunk0' in
      let chunk1 = chunk1 lor chunk1' in
      let chunk2 = chunk2 lor chunk2' in
      let chunk3 = chunk3 lor chunk3' in
      #(chunk0, chunk1, chunk2, chunk3)
    ;;

    let is_zero #(mask0, mask1, mask2, mask3) =
      let open I64.O in
      let mask = Vec.(mask0 lor mask1 lor mask2 lor mask3) in
      let min = Vec.minv mask in
      min = #0x0L
    ;;

    let find_offset_of_lsb #(mask0, mask1, mask2, mask3) =
      let open I64.O in
      let matches0 = Vec.movemask mask0 in
      let matches1 = Vec.movemask mask1 in
      let matches2 = Vec.movemask mask2 in
      let matches3 = Vec.movemask mask3 in
      let #(matches, extra_offset) =
        if matches0 <> #0L
        then #(matches0, #0L)
        else if matches1 <> #0L
        then #(matches1, #16L)
        else if matches2 <> #0L
        then #(matches2, #32L)
        else (
          [%debug_assert matches3 <> #0L];
          #(matches3, #48L))
      in
      (I64.ctz matches lsr 2) + extra_offset
    ;;
  end

  let memchr s ~pos ~len c =
    let open I64.O in
    let[@inline always] rec aux_serial s c =
      if String.Ptr.in_range s
      then (
        let chunk = String.Ptr.unsafe_get_char s |> Char.to_int |> I64.of_int in
        if not (I64.equal chunk c) then aux_serial (String.Ptr.advance s #1L) c else s)
      else s
    in
    let rec aux_simd s c_mask c =
      if String.Ptr.in_range (String.Ptr.advance s (Unrolled_vec.vector_size () - #1L))
      then (
        let chunk = Unrolled_vec.load s in
        let mask = Unrolled_vec.compare chunk c_mask in
        if Unrolled_vec.is_zero mask
        then aux_simd (String.Ptr.advance s (Unrolled_vec.vector_size ())) c_mask c
        else (
          let offset = Unrolled_vec.find_offset_of_lsb mask in
          String.Ptr.advance s offset))
      else aux_serial s c
    in
    let c = I64.of_int (Char.to_int c) in
    let res =
      let c_mask = Vec.set1 c in
      aux_simd (String.root s ~pos ~len) c_mask c
    in
    String.Ptr.finalize res s
  ;;

  let memchr2 s ~pos ~len c0 c1 =
    let open I64.O in
    let[@inline always] rec aux_serial s c0 c1 =
      if String.Ptr.in_range s
      then (
        let chunk = String.Ptr.unsafe_get_char s |> Char.to_int |> I64.of_int in
        if (not (I64.equal chunk c0)) && not (I64.equal chunk c1)
        then aux_serial (String.Ptr.advance s #1L) c0 c1
        else s)
      else s
    in
    let rec aux_simd s c0_mask c1_mask c0 c1 =
      if String.Ptr.in_range (String.Ptr.advance s (Unrolled_vec.vector_size () - #1L))
      then (
        let chunk = Unrolled_vec.load s in
        let mask = Unrolled_vec.(compare chunk c0_mask lor compare chunk c1_mask) in
        if Unrolled_vec.is_zero mask
        then
          aux_simd
            (String.Ptr.advance s (Unrolled_vec.vector_size ()))
            c0_mask
            c1_mask
            c0
            c1
        else (
          let offset = Unrolled_vec.find_offset_of_lsb mask in
          String.Ptr.advance s offset))
      else aux_serial s c0 c1
    in
    let c0 = I64.of_int (Char.to_int c0) in
    let c1 = I64.of_int (Char.to_int c1) in
    let res =
      let c0_mask = Vec.set1 c0 in
      let c1_mask = Vec.set1 c1 in
      aux_simd (String.root s ~pos ~len) c0_mask c1_mask c0 c1
    in
    String.Ptr.finalize res s
  ;;

  let memchr3 s ~pos ~len c0 c1 c2 =
    let open I64.O in
    let[@inline always] rec aux_serial s c0 c1 c2 =
      if String.Ptr.in_range s
      then (
        let chunk = String.Ptr.unsafe_get_char s |> Char.to_int |> I64.of_int in
        if (not (I64.equal chunk c0))
           && (not (I64.equal chunk c1))
           && not (I64.equal chunk c2)
        then aux_serial (String.Ptr.advance s #1L) c0 c1 c2
        else s)
      else s
    in
    let rec aux_simd s c0_mask c1_mask c2_mask c0 c1 c2 =
      if String.Ptr.in_range (String.Ptr.advance s (Unrolled_vec.vector_size () - #1L))
      then (
        let chunk = Unrolled_vec.load s in
        let mask =
          Unrolled_vec.(
            compare chunk c0_mask lor compare chunk c1_mask lor compare chunk c2_mask)
        in
        if Unrolled_vec.is_zero mask
        then
          aux_simd
            (String.Ptr.advance s (Unrolled_vec.vector_size ()))
            c0_mask
            c1_mask
            c2_mask
            c0
            c1
            c2
        else (
          let offset = Unrolled_vec.find_offset_of_lsb mask in
          String.Ptr.advance s offset))
      else aux_serial s c0 c1 c2
    in
    let c0 = I64.of_int (Char.to_int c0) in
    let c1 = I64.of_int (Char.to_int c1) in
    let c2 = I64.of_int (Char.to_int c2) in
    let res =
      let c0_mask = Vec.set1 c0 in
      let c1_mask = Vec.set1 c1 in
      let c2_mask = Vec.set1 c2 in
      aux_simd (String.root s ~pos ~len) c0_mask c1_mask c2_mask c0 c1 c2
    in
    String.Ptr.finalize res s
  ;;
end
[@@inline always]

open Ocaml_simd_neon

module Vec = struct
  include Int8x16

  let ( = ) x y = x = y
  let ( lor ) x y = x lor y

  let movemask t =
    (* Hack to get a bitmask

       https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon *)
    Int16x8.vshrn_n 4 (Int16x8.of_int8x16_bits t) |> Int64x2.extract 0
  ;;

  let size_bytes () = #16L
end

module String =
  Make [@kind value & bits64 & bits64]
    (Vec)
    (struct
      type t = string

      module Ptr = struct
        type t = #(string * pos:i64 * len:i64)

        let in_range #(_, ~pos, ~len) =
          let open I64.O in
          pos < len
        ;;

        let unsafe_get_vec (#(str, ~pos:byte, ~len:_) as t) =
          [%debug_assert in_range t];
          Int8x16.String.Int64_u.unsafe_get str ~byte
        ;;

        let unsafe_get_char (#(str, ~pos:byte, ~len:_) as t) =
          [%debug_assert in_range t];
          String.unsafe_get str (I64.to_int_trunc byte)
        ;;

        let is_null #(_, ~pos, ~len) =
          let open I64.O in
          pos = len
        ;;

        let advance #(t, ~pos, ~len) offset : t =
          let open I64.O in
          let t' : t = #(t, ~pos:(pos + offset), ~len) in
          t'
        ;;

        let finalize (#(_, ~pos, ~len:_) as t) _ =
          if is_null t then I64.Option.none () else I64.Option.unchecked_some pos
        ;;
      end

      let root t ~pos ~len = #(t, ~pos, ~len)
    end)

module Bytes =
  Make [@kind value & bits64 & bits64]
    (Vec)
    (struct
      type t = Bytes.t

      module Ptr = struct
        type t = #(Bytes.t * pos:i64 * len:i64)

        let unsafe_get_vec #(t, ~pos:byte, ~len:_) =
          Int8x16.Bytes.Int64_u.unsafe_get t ~byte
        ;;

        let unsafe_get_char #(t, ~pos:byte, ~len:_) =
          Bytes.unsafe_get t (I64.to_int_trunc byte)
        ;;

        let in_range #(_, ~pos, ~len) =
          let open I64.O in
          pos < len
        ;;

        let is_null #(_, ~pos, ~len) =
          let open I64.O in
          pos = len
        ;;

        let advance #(t, ~pos, ~len) offset : t =
          let open I64.O in
          let t' : t = #(t, ~pos:(pos + offset), ~len) in
          [%debug_assert not (is_null t')];
          t'
        ;;

        let finalize (#(_, ~pos, ~len:_) as t) _ =
          if is_null t then I64.Option.none () else I64.Option.unchecked_some pos
        ;;
      end

      let root t ~pos ~len = #(t, ~pos, ~len)
    end)

external unsafe_iptr_of_bigstring
  :  Base_bigstring.t
  -> pos:i64
  -> (nativeint#[@unboxed])
  = "caml_native_pointer_of_bigstring_bytecode" "caml_native_pointer_of_bigstring"
[@@noalloc] [@@no_effects] [@@no_coeffects]

external iptr_load_untagged_char
  :  (iptr[@unboxed])
  -> (char[@untagged])
  = "caml_native_pointer_load_untagged_char_bytecode"
    "caml_native_pointer_load_unsigned_int8"
[@@noalloc] [@@no_effects] [@@builtin]

module Bigstring =
  Make [@kind word & word & word]
    (Vec)
    (struct
      type t = Base_bigstring.t

      module Ptr = struct
        type t =
          #(pos:Iptr.Hex_unsigned.t
           * start:Iptr.Hex_unsigned.t
           * end_:Iptr.Hex_unsigned.t)

        let in_range #(~pos, ~start, ~end_) = Iptr.O.(pos >= start && pos < end_)

        let unsafe_get_vec (#(~pos, ~start:_, ~end_:_) as t) =
          [%debug_assert in_range t];
          Int8x16.Raw.unaligned_load pos
        ;;

        let unsafe_get_char ((#(~pos, ..) : t) as t) =
          [%debug_assert in_range t];
          iptr_load_untagged_char pos
        ;;

        let advance #(~pos, ~start, ~end_) by =
          let pos = Iptr.O.(pos + Iptr.of_i64_trunc by) in
          #(~pos, ~start, ~end_)
        ;;

        let finalize (#(~pos, ~start, ~end_:_) as t) buf =
          let offset =
            if in_range t
            then I64.Option.unchecked_some Iptr.O.(pos - start |> Iptr.to_i64)
            else I64.Option.none ()
          in
          (* This [opaque_identity] is load bearing. It ensures that the bigstring is live for the entire
             duration of the loop. *)
          Sys.opaque_identity buf |> (ignore : Base_bigstring.t -> unit);
          offset
        ;;
      end

      let root t ~pos ~len =
        let start = unsafe_iptr_of_bigstring t ~pos in
        let end_ = Iptr.O.(start + Iptr.of_i64_trunc len) in
        #(~pos:start, ~start, ~end_)
      ;;
    end)
