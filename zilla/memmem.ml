open! Base
open! Unboxed

module%template
  [@kind k = (word & word & word, value & bits64 & bits64)] Make
    (Vec : sig
     @@ portable
       type t : vec128 [@@deriving sexp_of]

       val ( = ) : t -> t -> t
       val ( land ) : t -> t -> t
       val movemask : t -> i64
       val minv : t -> i64
       val set1 : i64 -> t
       val size_bytes : unit -> i64
     end)
    (String : sig
     @@ portable
       type t

       module Ptr : sig
         type str := t
         type t : k [@@deriving sexp_of]

         val unsafe_get_vec : t -> Vec.t
         val unsafe_get_char : t -> char
         val advance : t -> i64 -> t
         val not_past_end : t -> bool
         val finalize : t -> str -> I64.Option.t
       end

       val root : t -> pos:i64 -> len:i64 -> Ptr.t
     end) =
struct
  let _to_string ptr ~len =
    Core.String.init (I64.to_int_trunc len) ~f:(fun i ->
      String.Ptr.unsafe_get_char (String.Ptr.advance ptr (I64.of_int i)))
  ;;

  let[@loopify] rec memcmp a b ~len =
    let open I64.O in
    if len <= #0L
    then true
    else
      Char.equal (String.Ptr.unsafe_get_char a) (String.Ptr.unsafe_get_char b)
      && memcmp (String.Ptr.advance a #1L) (String.Ptr.advance b #1L) ~len:(len - #1L)
  ;;

  let memmem
    ~needle
    ~needle_pos
    ~needle_len
    ~haystack:haystack_str
    ~haystack_pos
    ~haystack_len
    =
    let open I64.O in
    let needle = String.root needle ~pos:needle_pos ~len:needle_len in
    let haystack = String.root haystack_str ~pos:haystack_pos ~len:haystack_len in
    [%debug_assert needle_len >= #0L];
    [%debug_assert haystack_len >= #0L];
    if needle_len = #0L
    then I64.Option.some #0L
    else if haystack_len < needle_len
    then I64.Option.none ()
    else (
      (* TODO: clean up char -> i64 -> vec conversions *)
      let first_char =
        String.Ptr.unsafe_get_char needle |> Char.to_int |> I64.of_int |> Vec.set1
      in
      let last_char =
        String.Ptr.unsafe_get_char (String.Ptr.advance needle (needle_len - #1L))
        |> Char.to_int
        |> I64.of_int
        |> Vec.set1
      in
      let[@loopify] rec aux_serial haystack =
        if String.Ptr.not_past_end (String.Ptr.advance haystack (needle_len - #1L))
        then (
          let[@loopify] rec loop haystack needle len =
            if len <= #0L
            then true
            else
              Char.equal
                (String.Ptr.unsafe_get_char needle)
                (String.Ptr.unsafe_get_char haystack)
              && loop
                   (String.Ptr.advance haystack #1L)
                   (String.Ptr.advance needle #1L)
                   (len - #1L)
          in
          if loop haystack needle needle_len
          then haystack
          else aux_serial (String.Ptr.advance haystack #1L))
        else String.Ptr.advance haystack needle_len
      in
      let[@loopify] rec aux_simd haystack =
        if String.Ptr.not_past_end
             (String.Ptr.advance haystack (needle_len - #1L + Vec.size_bytes () - #1L))
        then (
          let block_first = String.Ptr.unsafe_get_vec haystack in
          let block_last =
            String.Ptr.unsafe_get_vec (String.Ptr.advance haystack (needle_len - #1L))
          in
          let eq_first = Vec.(block_first = first_char) in
          let eq_last = Vec.(block_last = last_char) in
          let candidates = Vec.(eq_first land eq_last) in
          let contains_candidates = Vec.minv candidates = #0xFFL in
          if contains_candidates
          then (
            let candidates = Vec.movemask candidates in
            let[@loopify] rec aux candidates i =
              if candidates = #0L
              then I64.Option.none ()
              else if candidates land #1L = #1L
                      && memcmp
                           (String.Ptr.advance haystack (i + #1L))
                           (String.Ptr.advance needle #1L)
                           ~len:(needle_len - #2L)
              then I64.Option.some i
              else aux (candidates lsr 4) (i + #1L)
            in
            match%optional_u.I64.Option aux candidates #0L with
            | None -> aux_simd (String.Ptr.advance haystack (Vec.size_bytes ()))
            | Some offset -> String.Ptr.advance haystack offset)
          else aux_simd (String.Ptr.advance haystack (Vec.size_bytes ())))
        else aux_serial haystack
      in
      let haystack = aux_simd haystack in
      String.Ptr.finalize haystack haystack_str)
  ;;
end
[@@inline always]

open Ocaml_simd_neon

module Vec = struct
  include Int8x16

  let ( = ) x y = x = y
  let ( land ) x y = x land y

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
        type t = #(string * pos:i64 * len:i64) [@@deriving sexp_of]

        let in_range #(_, ~pos, ~len) =
          let open I64.O in
          pos >= #0L && pos < len
        ;;

        let not_past_end #(_, ~pos, ~len) =
          let open I64.O in
          [%debug_assert pos >= #0L];
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

        let advance #(t, ~pos, ~len) offset : t =
          let open I64.O in
          let t' : t = #(t, ~pos:(pos + offset), ~len) in
          t'
        ;;

        let finalize (#(_, ~pos, ~len:_) as t) _ =
          if not (in_range t) then I64.Option.none () else I64.Option.unchecked_some pos
        ;;
      end

      let root t ~pos ~len = #(t, ~pos, ~len)
    end)

module Bytes =
  Make [@kind value & bits64 & bits64]
    (Vec)
    (struct
      type t = Bytes.t [@@deriving sexp_of]

      module Ptr = struct
        type t = #(Bytes.t * pos:i64 * len:i64) [@@deriving sexp_of]

        let in_range #(_, ~pos, ~len) =
          let open I64.O in
          pos >= #0L && pos < len
        ;;

        let not_past_end #(_, ~pos, ~len) =
          let open I64.O in
          [%debug_assert pos >= #0L];
          pos < len
        ;;

        let unsafe_get_vec (#(str, ~pos:byte, ~len:_) as t) =
          [%debug_assert in_range t];
          Int8x16.Bytes.Int64_u.unsafe_get str ~byte
        ;;

        let unsafe_get_char (#(str, ~pos:byte, ~len:_) as t) =
          [%debug_assert in_range t];
          Bytes.unsafe_get str (I64.to_int_trunc byte)
        ;;

        let advance #(t, ~pos, ~len) offset : t =
          let open I64.O in
          let t' : t = #(t, ~pos:(pos + offset), ~len) in
          t'
        ;;

        let finalize (#(_, ~pos, ~len:_) as t) _ =
          if not (in_range t) then I64.Option.none () else I64.Option.unchecked_some pos
        ;;
      end

      let root t ~pos ~len = #(t, ~pos, ~len)
    end)

external unsafe_iptr_of_bigstring
  :  Base_bigstring.t
  -> pos:i64
  -> (nativeint#[@unboxed])
  @@ portable
  = "caml_native_pointer_of_bigstring_bytecode" "caml_native_pointer_of_bigstring"
[@@noalloc] [@@no_effects] [@@no_coeffects]

external iptr_load_untagged_char
  :  (iptr[@unboxed])
  -> (char[@untagged])
  @@ portable
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
        [@@deriving sexp_of]

        let sexp_of_t #(~pos, ~start, ~end_) =
          let open Iptr.O in
          let pos = pos - start in
          let end_ = end_ - start in
          let start = #0n in
          sexp_of_t #(~pos, ~start, ~end_)
        ;;

        let advance #(~pos, ~start, ~end_) by =
          let pos = Iptr.O.(pos + Iptr.of_i64_trunc by) in
          #(~pos, ~start, ~end_)
        ;;

        let not_past_end #(~pos, ~start, ~end_) =
          [%debug_assert Iptr.O.(pos >= start)];
          Iptr.O.(pos < end_)
        ;;

        let in_range #(~pos, ~start, ~end_) = Iptr.O.(pos >= start && pos < end_)

        let unsafe_get_vec (#(~pos, ~start:_, ~end_:_) as t) =
          [%debug_assert in_range t && in_range (advance t #15L)];
          Int8x16.Raw.unaligned_load pos
        ;;

        let unsafe_get_char ((#(~pos, ..) : t) as t) =
          [%debug_assert in_range t];
          iptr_load_untagged_char pos
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
