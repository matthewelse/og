open! Core

module type S = sig
  type t [@@deriving sexp_of]

  val substring : t -> pos:int64# -> len:int64# -> string
  val length : t -> int64#
  val output : t -> Out_channel.t -> pos:int -> len:int -> unit
  val unsafe_get : t -> int64# -> char
  val unsafe_get_i64 : t -> int64# -> int64#
  val to_string : t -> string
  val of_string : string @ local -> t
end

module Make (Data : S) = struct
  type t =
    { (* If you change this, update the implementation of [slice_stubs.c]. *)
      global_ bytes : Data.t
    ; pos : (I64.t[@globalized])
    ; len : (I64.t[@globalized])
    }
  [@@deriving globalize, sexp_of]

  module Expert = struct
    let bytes (local_ t) = t.bytes
    let pos t = t.pos
    let len t = t.len
  end

  let to_string t = Data.substring t.bytes ~pos:t.pos ~len:t.len

  let print_endline t =
    Out_channel.output_substring
      stdout
      ~buf:(Data.to_string t.bytes)
      ~pos:(I64.to_int_trunc t.pos)
      ~len:(I64.to_int_trunc t.len);
    Out_channel.newline stdout
  ;;

  let check_bounds t =
    let open I64.O in
    if t.pos + t.len > Data.length t.bytes
    then (
      let t = globalize t in
      raise_s [%message "Start index+length out of bounds" (t : t)])
    else if t.pos < #0L
    then (
      let t = globalize t in
      raise_s [%message "Start position < 0" (t : t)])
    else if t.len < #0L
    then (
      let t = globalize t in
      raise_s [%message "Length < 0" (t : t)])
  ;;

  let unsafe_create bytes ~pos ~len = { bytes; pos; len }
  let unsafe_create_local bytes ~pos ~len = exclave_ { bytes; pos; len }

  let create bytes ~pos ~len =
    let t = { bytes; pos; len } in
    check_bounds t;
    t
  ;;

  let create_local bytes ~pos ~len = exclave_
    let local_ t = { bytes; pos; len } in
    check_bounds t;
    t
  ;;

  let of_string s =
    let d = Data.of_string s in
    unsafe_create d ~pos:#0L ~len:(Data.length d)
  ;;

  let length t = t.len

  let at_exn (local_ t) ix =
    let open I64.O in
    if ix >= t.len || ix < #0L
    then failwith "index out of bounds"
    else Data.unsafe_get t.bytes (t.pos + ix)
  ;;

  let at (local_ t) ix =
    let open I64.O in
    if ix >= t.len || ix < #0L
    then None
    else exclave_ Some (Data.unsafe_get t.bytes (t.pos + ix))
  ;;

  let unsafe_at (local_ t) ix =
    let open I64.O in
    Data.unsafe_get t.bytes (t.pos + ix)
  ;;

  let iter t ~f =
    let open I64.O in
    for%i64 i = #0L to t.len - #1L do
      f (Data.unsafe_get t.bytes (i + t.pos))
    done
  ;;

  let iteri t ~f =
    let open I64.O in
    for%i64 i = #0L to t.len - #1L do
      f i (Data.unsafe_get t.bytes (i + t.pos))
    done
  ;;

  let[@inline always] unsafe_slice t ~pos ~len = exclave_
    let open I64.O in
    { t with pos = t.pos + pos; len }
  ;;

  let slice t ~pos ~len =
    let open I64.O in
    if pos < #0L || len < #0L || pos + len > t.len
    then None
    else exclave_ Some (unsafe_slice ~pos ~len t)
  ;;

  let slice_exn t ~pos ~len =
    let open I64.O in
    if pos < #0L || len < #0L || pos + len > t.len
    then failwith "index out of bounds"
    else exclave_ unsafe_slice ~pos ~len t
  ;;

  let rec memchr_slow_path t c ~offset : I64.Option.t =
    let open I64.O in
    if offset < t.len
    then
      if Char.equal (unsafe_at t offset) c
      then exclave_ Some offset
      else exclave_ memchr_slow_path t c ~offset:(offset + #1L)
    else None
  ;;

  let rec memchr_fast_path t c mask_c ~offset : I64.Option.t =
    (* Adapted from https://bits.stephan-brumme.com/null.html *)
    let open I64.O in
    let mask_lo = #0x0101010101010101L in
    let mask_hi = #0x8080808080808080L in
    if offset + #8L <= t.len
    then (
      let bytes = Data.unsafe_get_i64 t.bytes (t.pos + offset) in
      let with_c_zeros = I64.O.(bytes lxor mask_c) in
      let res = I64.O.((with_c_zeros - mask_lo) land lnot with_c_zeros land mask_hi) in
      if I64.O.(res <> #0L)
      then (
        let byte_offset = I64.ctz res lsr 3 in
        exclave_ Some (offset + I64.of_int byte_offset))
      else exclave_ memchr_fast_path t c mask_c ~offset:(offset + #8L))
    else exclave_ memchr_slow_path t c ~offset
  ;;

  let memchr t c : I64.Option.t = exclave_ memchr_fast_path t c (I64.splat c) ~offset:#0L

  let rec memcmp_slow_path t t' ~offset ~length =
    let open I64.O in
    offset = length
    || (offset < length
        && Char.equal (unsafe_at t offset) (unsafe_at t' offset)
        && memcmp_slow_path t t' ~offset:(offset + #1L) ~length)
  ;;

  let rec memcmp_fast_path t t' ~offset ~length =
    let open I64.O in
    if offset + #8L <= length
    then (
      let bytes = Data.unsafe_get_i64 t.bytes (t.pos + offset) in
      let bytes' = Data.unsafe_get_i64 t'.bytes (t'.pos + offset) in
      bytes = bytes' && memcmp_fast_path t t' ~offset:(offset + #8L) ~length)
    else memcmp_slow_path t t' ~offset ~length
  ;;

  let memcmp (local_ t) (local_ t') =
    let open I64.O in
    t.len = t'.len && memcmp_fast_path t t' ~offset:#0L ~length:t.len
  ;;

  let output t out_channel =
    Data.output
      t.bytes
      out_channel
      ~pos:(I64.to_int_trunc t.pos)
      ~len:(I64.to_int_trunc t.len)
  ;;

  module KMP = struct
    type slice = t [@@deriving sexp_of]

    type t =
      { pattern : slice
      ; offsets : (int64# Iarray.t[@sexp.opaque])
      }
    [@@deriving sexp_of]

    let pattern t = t.pattern

    let create pattern =
      let open I64.O in
      let length = I64.to_int_trunc (length pattern) in
      let offsets =
        Iarray.construct__bits64 ~len:length ~default:#0L ~f:(fun offsets ->
          let local_ k = I64.Ref.create_local #0L in
          for i = 1 to Int.(length - 1) do
            let i = I64.of_int i in
            while !k > #0L && not (Char.equal (at_exn pattern !k) (at_exn pattern i)) do
              I64.Ref.set k (I64.Array.get offsets (!k - #1L))
            done;
            if Char.equal (at_exn pattern !k) (at_exn pattern i) then I64.Ref.incr k;
            I64.Array.set offsets i !k
          done)
      in
      { pattern; offsets }
    ;;

    let indexes t haystack ~f =
      let open I64.O in
      if length t.pattern = #0L
      then
        for%i64 i = #0L to length haystack do
          f i
        done
      else (
        let local_ q = I64.Ref.create_local #0L in
        for%i64 i = #0L to length haystack - #1L do
          while !q > #0L && not (Char.equal (at_exn t.pattern !q) (at_exn haystack i)) do
            I64.Ref.set q (I64.Iarray.get t.offsets (!q - #1L))
          done;
          if Char.equal (at_exn t.pattern !q) (at_exn haystack i) then incr q;
          if !q = length t.pattern
          then (
            f (i - length t.pattern + #1L);
            I64.Ref.set q (I64.Iarray.get t.offsets (!q - #1L)))
        done)
    ;;

    let index t haystack : I64.Option.t =
      let open I64.O in
      let result = I64.Ref.create_local (-#1L) in
      With_return.with_return (fun { return } ->
        indexes t haystack ~f:(fun pos ->
          I64.Ref.set result pos;
          return ()) [@nontail]);
      let result = !result in
      if result = -#1L then None else exclave_ Some result
    ;;
  end

  module BMH = struct
    type slice = t [@@deriving sexp_of]

    type t =
      { pattern : slice
      ; offsets : (int64# Iarray.t[@sexp.opaque])
      }
    [@@deriving sexp_of]

    let pattern t = t.pattern

    let create pattern =
      let open I64.O in
      let offsets =
        Iarray.construct__bits64 ~len:256 ~default:(length pattern) ~f:(fun offsets ->
          iteri pattern ~f:(fun i c ->
            if i <> length pattern - #1L
            then
              I64.Array.set
                offsets
                (Char.to_int c |> I64.of_int)
                (length pattern - i - #1L)) [@nontail])
      in
      { pattern; offsets }
    ;;

    let get_jump t c = Iarray.unsafe_get t.offsets (Char.to_int c)

    let rec indexes_from t haystack ~f ~offset =
      (* Boyer-Moore-Horspool string matching *)
      let open I64.O in
      match slice haystack ~pos:offset ~len:t.pattern.len with
      | None -> ()
      | Some haystack_slice ->
        if memcmp haystack_slice t.pattern then f offset;
        let last_character_of_haystack_slice =
          (* SAFETY: [haystack_slice] has length [t.pattern.len] *)
          unsafe_at haystack_slice (t.pattern.len - #1L)
        in
        let offset = offset + get_jump t last_character_of_haystack_slice in
        indexes_from t haystack ~f ~offset
    ;;

    let indexes t haystack ~f =
      let open I64.O in
      if length t.pattern = #0L
      then
        for%i64 i = #0L to length haystack do
          f i
        done
      else indexes_from t haystack ~f ~offset:#0L
    ;;

    let index t haystack : I64.Option.t =
      let open I64.O in
      let result = I64.Ref.create_local (-#1L) in
      With_return.with_return (fun { return } ->
        indexes t haystack ~f:(fun pos ->
          I64.Ref.set result pos;
          return ()) [@nontail]);
      let result = !result in
      if result = -#1L then None else exclave_ Some result
    ;;
  end

  module Boyer_moore = struct
    type slice = t [@@deriving sexp_of]

    type t =
      { bad_character : (int64# Iarray.t[@sexp.opaque])
      ; bad_suffix : (int64# Iarray.t[@sexp.opaque])
      ; pattern : slice
      }
    [@@deriving fields ~getters, sexp_of]

    let compute_bad_suffix_table pattern =
      (* TODO: make this more efficient and comprehensible *)
      let pattern = to_string pattern in
      Iarray.construct__bits64 ~len:(String.length pattern) ~default:#0L ~f:(fun table ->
        let at s i = if i < 0 || i >= String.length s then None else Some s.[i] in
        for suffix_length = 0 to String.length pattern - 1 do
          With_return.with_return (fun { return } ->
            for offset = 1 to String.length pattern do
              let suffix = String.suffix pattern suffix_length in
              let prefix = String.prefix pattern (String.length pattern - offset) in
              let prefix_suffix = String.suffix prefix suffix_length in
              if String.is_suffix ~suffix:prefix_suffix suffix
                 && not
                      (Option.equal__local
                         Char.equal__local
                         (at pattern (String.length pattern - 1 - suffix_length))
                         (at pattern (String.length pattern - 1 - suffix_length - offset)))
              then (
                Og_utils__Array.set
                  table
                  (String.length pattern - 1 - suffix_length)
                  (I64.of_int (suffix_length + offset));
                return ())
            done)
        done)
    ;;

    let compute_bad_character_table pattern =
      let pattern = to_string pattern in
      let pattern_length = String.length pattern |> I64.of_int in
      Iarray.construct__bits64
        ~len:256
        ~default:pattern_length
        ~f:(fun bad_character_table ->
          for i = 0 to String.length pattern - 1 do
            Og_utils__Array.set
              bad_character_table
              (Char.to_int pattern.[i])
              I64.O.(pattern_length - #1L - I64.of_int i)
          done)
    ;;

    let create pattern =
      let bad_suffix = compute_bad_suffix_table pattern in
      let bad_character = compute_bad_character_table pattern in
      { bad_suffix; bad_character; pattern }
    ;;

    let indexes t haystack ~f =
      let open I64.O in
      if length t.pattern = #0L
      then
        for%i64 i = #0L to length haystack do
          f i
        done
      else (
        let local_ offset = I64.Ref.create_local (length t.pattern - #1L) in
        while
          match slice haystack ~pos:!offset ~len:(length haystack - !offset) with
          | None -> false
          | Some slice ->
            (match memchr slice (unsafe_at t.pattern (length t.pattern - #1L)) with
             | None -> false
             | Some step_by ->
               I64.Ref.add offset step_by;
               !offset >= #0L && !offset < length haystack)
        do
          let local_ pattern_offset = I64.Ref.create_local (length t.pattern - #1L) in
          while
            !pattern_offset >= #0L
            && Char.equal
                 (unsafe_at t.pattern !pattern_offset)
                 (unsafe_at haystack !offset)
          do
            (* Search right to left through the pattern/haystack *)
            I64.Ref.decr pattern_offset;
            I64.Ref.decr offset
          done;
          if !pattern_offset < #0L
          then (
            f (!offset + #1L);
            (* FIXME: there is a jump we can do here, I'm just not sure what it is. *)
            (* Advance by one if we have a successful match *)
            I64.Ref.add offset (length t.pattern + #1L))
          else
            I64.Ref.add
              offset
              (max
                 (Iarray.unsafe_get
                    t.bad_character
                    (Char.to_int (unsafe_at haystack !offset)))
                 (I64.Iarray.unsafe_get t.bad_suffix !pattern_offset))
        done)
    ;;

    let index t haystack : I64.Option.t =
      let open I64.O in
      let result = I64.Ref.create_local (-#1L) in
      With_return.with_return (fun { return } ->
        indexes t haystack ~f:(fun pos ->
          I64.Ref.set result pos;
          return ()) [@nontail]);
      let result = !result in
      if result = -#1L then None else exclave_ Some result
    ;;
  end

  module Search_pattern = Boyer_moore
end
[@@inline always]

module Bytes = struct
  include Make (struct
      include Bytes

      let substring t ~pos ~len =
        let pos = I64.to_int_trunc pos in
        let len = I64.to_int_trunc len in
        Bytes.sub t ~pos ~len |> to_string
      ;;

      let to_string t = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t

      external unsafe_get_i64
        :  (t[@local_opt])
        -> int64#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int64#"
      [@@noalloc]

      let length t = I64.of_int (length t)
      let unsafe_get t n = unsafe_get t (I64.to_int_trunc n)
      let of_string s = Bytes.of_string (String.globalize s)

      let output t channel ~pos ~len =
        Out_channel.output_substring
          channel
          ~pos
          ~len
          ~buf:(Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t)
      ;;
    end)

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Bytes.unsafe_blit
      ~src:src.bytes
      ~src_pos:(I64.to_int_trunc src.pos + src_pos)
      ~dst:dst.bytes
      ~dst_pos:(I64.to_int_trunc dst.pos + dst_pos)
      ~len
  ;;

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    if src_pos < 0
       || len < 0
       || src_pos + len > I64.to_int_trunc src.len
       || dst_pos < 0
       || dst_pos + len > I64.to_int_trunc dst.len
    then failwith "index out of bounds"
    else unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;
end

module String = struct
  include Make (struct
      include String

      let substring t ~pos ~len =
        sub t ~pos:(I64.to_int_trunc pos) ~len:(I64.to_int_trunc len)
      ;;

      let to_string t = t

      external unsafe_get_i64
        :  (t[@local_opt])
        -> int64#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int64#"
      [@@noalloc]

      let length t = I64.of_int (length t)
      let unsafe_get t n = unsafe_get t (I64.to_int_trunc n)
      let of_string s = globalize s

      let output t channel ~pos ~len =
        Out_channel.output_substring channel ~pos ~len ~buf:t
      ;;
    end)
end

module Bigstring = struct
  include Make (struct
      include Bigstring

      let substring t ~pos ~len =
        let sub =
          sub_shared_local ~pos:(I64.to_int_trunc pos) ~len:(I64.to_int_trunc len) t
        in
        to_string sub [@nontail]
      ;;

      let to_string t = to_string t

      external unsafe_get_i64
        :  (t[@local_opt])
        -> int64#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int64#"
      [@@noalloc]

      let[@inline always] unsafe_get t n = unsafe_get t (I64.to_int_trunc n)
      let length t = length t |> I64.of_int
      let of_string s = of_string s
      let output t channel ~pos ~len = Bigstring_unix.really_output stdout ~pos ~len t
    end)

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Bigstring.unsafe_blit
      ~src:src.bytes
      ~src_pos:(I64.to_int_trunc src.pos + src_pos)
      ~dst:dst.bytes
      ~dst_pos:(I64.to_int_trunc dst.pos + dst_pos)
      ~len
  ;;

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    if src_pos < 0
       || len < 0
       || src_pos + len > I64.to_int_trunc src.len
       || dst_pos < 0
       || dst_pos + len > I64.to_int_trunc dst.len
    then failwith "index out of bounds"
    else unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;
end

include Bigstring
