open! Core

module type S = sig
  type t [@@deriving sexp_of]

  val substring : t -> pos:int64# -> len:int64# -> string
  val length : t -> int64#
  val unsafe_get : t -> int64# -> char
  val unsafe_get_u64 : t -> int64# -> int64#
  val unsafe_to_string : t -> string
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
      ~buf:(Data.unsafe_to_string t.bytes)
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

  let memchr t c : I64.Option.t =
    let open I64.O in
    (* Adapted from https://bits.stephan-brumme.com/null.html *)
    let result = I64.Ref.create_local (-#1L) in
    let offset = I64.Ref.create_local #0L in
    let length = length t in
    let `fast_path =
      let mask_lo = #0x0101010101010101L in
      let mask_hi = #0x8080808080808080L in
      let mask_c = I64.splat c in
      while !result = -#1L && !offset + #8L <= length do
        let bytes = Data.unsafe_get_u64 t.bytes (t.pos + !offset) in
        let with_c_zeros = I64.O.(bytes lxor mask_c) in
        let res = I64.O.((with_c_zeros - mask_lo) land lnot with_c_zeros land mask_hi) in
        if I64.O.(res <> #0L)
        then (
          let byte_offset = I64.ctz res lsr 3 in
          I64.Ref.set result (!offset + I64.of_int byte_offset));
        I64.Ref.add offset #8L
      done;
      `fast_path
    in
    let `slow_path =
      while !result = -#1L && !offset < length do
        if Char.equal (unsafe_at t !offset) c
        then I64.Ref.set result !offset
        else I64.Ref.add offset #1L
      done;
      `slow_path
    in
    let result = !result in
    if result = -#1L then None else exclave_ Some result
  ;;

  let memcmp (local_ t) (local_ t') =
    let open I64.O in
    t.len = t'.len
    &&
    let result = ref true in
    let offset = I64.Ref.create_local #0L in
    let length = t.len in
    let `fast_path =
      while Ref.(!result) && !offset + #8L <= length do
        let bytes = Data.unsafe_get_u64 t.bytes (t.pos + !offset) in
        let bytes' = Data.unsafe_get_u64 t'.bytes (t'.pos + !offset) in
        if I64.O.(bytes <> bytes') then result := false;
        I64.Ref.add offset #8L
      done;
      `fast_path
    in
    let `slow_path =
      while Ref.(!result) && !offset < length do
        if not (Char.equal (unsafe_at t !offset) (unsafe_at t' !offset))
        then result := false;
        I64.Ref.add offset #1L
      done;
      `slow_path
    in
    Ref.(!result)
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

module Bytes = struct
  include Make (struct
      include Bytes

      let substring t ~pos ~len =
        let pos = I64.to_int_trunc pos in
        let len = I64.to_int_trunc len in
        Bytes.sub t ~pos ~len |> to_string
      ;;

      let unsafe_to_string t =
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t
      ;;

      external unsafe_get_u64
        :  (t[@local_opt])
        -> int64#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int64#"
      [@@noalloc]

      let length t = I64.of_int (length t)
      let unsafe_get t n = unsafe_get t (I64.to_int_trunc n)
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

      let unsafe_to_string t = t

      external unsafe_get_u64
        :  (t[@local_opt])
        -> int64#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int64#"
      [@@noalloc]

      let length t = I64.of_int (length t)
      let unsafe_get t n = unsafe_get t (I64.to_int_trunc n)
    end)

  let of_string s = create ~pos:#0L ~len:(String.length s |> I64.of_int) s
end

include String
