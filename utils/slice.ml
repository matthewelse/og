open! Core

module type S = sig
  type t [@@deriving sexp_of]

  val sub_string : (t, string) Blit.sub_global
  val length : t -> int
  val unsafe_get : t -> int -> char
  val unsafe_to_string : t -> string
end

module Make (Data : S) = struct
  type t =
    { (* If you change this, update the implementation of [slice_stubs.c]. *)
      global_ bytes : Data.t
    ; pos : int
    ; len : int
    }
  [@@deriving globalize, sexp_of]

  module Expert = struct
    let bytes (local_ t) = t.bytes
    let pos t = t.pos
    let len t = t.len
  end

  let to_string t = Data.sub_string t.bytes ~pos:t.pos ~len:t.len

  let print_endline t =
    Out_channel.output_substring
      stdout
      ~buf:(Data.unsafe_to_string t.bytes)
      ~pos:t.pos
      ~len:t.len;
    Out_channel.newline stdout
  ;;

  let check_bounds t =
    if t.pos + t.len > Data.length t.bytes
    then (
      let t = globalize t in
      raise_s [%message "Start index+length out of bounds" (t : t)])
    else if t.pos < 0
    then (
      let t = globalize t in
      raise_s [%message "Start position < 0" (t : t)])
    else if t.len < 0
    then (
      let t = globalize t in
      raise_s [%message "Length < 0" (t : t)])
  ;;

  let create ?(pos = 0) ?len bytes =
    let len = Option.value len ~default:(Data.length bytes - pos) in
    let t = { bytes; pos; len } in
    check_bounds t;
    t
  ;;

  let create_local ?(pos = 0) ?len bytes = exclave_
    let len = Option.value len ~default:(Data.length bytes - pos) in
    let local_ t = { bytes; pos; len } in
    check_bounds t;
    t
  ;;

  let length t = t.len

  let at_exn (local_ t) ix =
    if ix >= t.len || ix < 0
    then failwith "index out of bounds"
    else Data.unsafe_get t.bytes (t.pos + ix)
  ;;

  let at (local_ t) ix =
    if ix >= t.len || ix < 0
    then None
    else exclave_ Some (Data.unsafe_get t.bytes (t.pos + ix))
  ;;

  let unsafe_at (local_ t) ix = Data.unsafe_get t.bytes (t.pos + ix)

  let iter t ~f =
    for i = 0 to t.len - 1 do
      f (Data.unsafe_get t.bytes (i + t.pos))
    done
  ;;

  let iteri t ~f =
    for i = 0 to t.len - 1 do
      f i (Data.unsafe_get t.bytes (i + t.pos))
    done
  ;;

  let[@inline always] unsafe_slice t ~pos ~len = exclave_
    { t with pos = t.pos + pos; len }
  ;;

  let slice t ~pos ~len =
    if pos < 0 || len < 0 || pos + len > t.len
    then None
    else exclave_ Some (unsafe_slice ~pos ~len t)
  ;;

  let slice_exn t ~pos ~len =
    if pos < 0 || len < 0 || pos + len > t.len
    then failwith "index out of bounds"
    else exclave_ unsafe_slice ~pos ~len t
  ;;

  external memchr_fast : t @ local -> char -> int = "slice_memchr" [@@noalloc]

  let memchr t c =
    match memchr_fast t c with
    | -1 -> None
    | i -> exclave_ Some i
  ;;

  let impl = `c_stub

  external memcmp_fast : t @ local -> t @ local -> bool = "slice_memcmp" [@@noalloc]

  let memcmp_iter (local_ t) (local_ t') =
    t.len = t'.len
    && (With_return.with_return (fun { return } ->
          for i = 0 to t.len - 1 do
            if not (Char.equal (unsafe_at t i) (unsafe_at t' i)) then return false
          done;
          true) [@nontail])
  ;;

  let memcmp_rec t t' =
    let rec aux t t' i =
      if i >= t.len
      then true
      else if not (Char.equal (unsafe_at t i) (unsafe_at t' i))
      then false
      else aux t t' (i + 1)
    in
    t.len = t'.len && aux t t' 0
  ;;

  let memcmp =
    (* Empirically, the OCaml implementations are ~identical, but the C stub is
       ~500ms faster at churning through the Linux kernel. *)
    match impl with
    | `c_stub -> memcmp_fast
    | `ocaml_iter -> memcmp_iter
    | `ocaml_rec -> memcmp_rec
  ;;

  module KMP = struct
    type slice = t [@@deriving sexp_of]

    type t =
      { pattern : slice
      ; offsets : (int Iarray.t[@sexp.opaque])
      }
    [@@deriving sexp_of]

    let pattern t = t.pattern

    let create pattern =
      let offsets =
        Iarray.construct ~len:(length pattern) ~default:0 ~f:(fun offsets ->
          let local_ k = ref 0 in
          for i = 1 to length pattern - 1 do
            while !k > 0 && not (Char.equal (at_exn pattern !k) (at_exn pattern i)) do
              k := offsets.(!k - 1)
            done;
            if Char.equal (at_exn pattern !k) (at_exn pattern i) then incr k;
            offsets.(i) <- !k
          done)
      in
      { pattern; offsets }
    ;;

    let indexes t haystack ~f =
      if length t.pattern = 0
      then
        for i = 0 to length haystack do
          f i
        done
      else (
        let local_ q = ref 0 in
        for i = 0 to length haystack - 1 do
          while !q > 0 && not (Char.equal (at_exn t.pattern !q) (at_exn haystack i)) do
            q := Iarray.get t.offsets (!q - 1)
          done;
          if Char.equal (at_exn t.pattern !q) (at_exn haystack i) then incr q;
          if !q = length t.pattern
          then (
            f (i - length t.pattern + 1);
            q := Iarray.get t.offsets (!q - 1))
        done)
    ;;

    let index t haystack =
      let result =
        With_return.with_return (fun { return } ->
          indexes t haystack ~f:return;
          -1)
      in
      if result = -1 then None else exclave_ Some result
    ;;
  end

  module BMH = struct
    type slice = t [@@deriving sexp_of]

    type t =
      { pattern : slice
      ; offsets : (int Iarray.t[@sexp.opaque])
      }
    [@@deriving sexp_of]

    let pattern t = t.pattern

    let create pattern =
      let offsets =
        Iarray.construct ~len:256 ~default:(length pattern) ~f:(fun offsets ->
          iteri pattern ~f:(fun i c ->
            if i <> length pattern - 1
            then offsets.(Char.to_int c) <- length pattern - i - 1) [@nontail])
      in
      { pattern; offsets }
    ;;

    let get_jump t c = Iarray.unsafe_get t.offsets (Char.to_int c)

    let rec indexes_from t haystack ~f ~offset =
      (* Boyer-Moore-Horspool string matching *)
      match slice haystack ~pos:offset ~len:t.pattern.len with
      | None -> ()
      | Some haystack_slice ->
        if memcmp haystack_slice t.pattern then f offset;
        let last_character_of_haystack_slice =
          (* SAFETY: [haystack_slice] has length [t.pattern.len] *)
          unsafe_at haystack_slice (t.pattern.len - 1)
        in
        let offset = offset + get_jump t last_character_of_haystack_slice in
        indexes_from t haystack ~f ~offset
    ;;

    let indexes t haystack ~f =
      if length t.pattern = 0
      then
        for i = 0 to length haystack do
          f i
        done
      else indexes_from t haystack ~f ~offset:0
    ;;

    let index t haystack =
      let result =
        With_return.with_return (fun { return } ->
          indexes t haystack ~f:return;
          -1)
      in
      if result = -1 then None else exclave_ Some result
    ;;
  end

  module Boyer_moore = struct
    type slice = t [@@deriving sexp_of]

    type t =
      { bad_character : (int Iarray.t[@sexp.opaque])
      ; bad_suffix : (int Iarray.t[@sexp.opaque])
      ; pattern : slice
      }
    [@@deriving fields ~getters, sexp_of]

    let compute_bad_suffix_table pattern =
      (* TODO: make this more efficient and comprehensible *)
      let pattern = to_string pattern in
      Iarray.construct ~len:(String.length pattern) ~default:0 ~f:(fun table ->
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
                table.(String.length pattern - 1 - suffix_length)
                <- suffix_length + offset;
                return ())
            done)
        done)
    ;;

    let compute_bad_character_table pattern =
      let pattern = to_string pattern in
      Iarray.construct
        ~len:256
        ~default:(String.length pattern)
        ~f:(fun bad_character_table ->
          for i = 0 to String.length pattern - 1 do
            bad_character_table.(Char.to_int pattern.[i]) <- String.length pattern - 1 - i
          done)
    ;;

    let create pattern =
      let bad_suffix = compute_bad_suffix_table pattern in
      let bad_character = compute_bad_character_table pattern in
      { bad_suffix; bad_character; pattern }
    ;;

    let indexes t haystack ~f =
      if length t.pattern = 0
      then
        for i = 0 to length haystack do
          f i
        done
      else (
        let local_ offset = ref (length t.pattern - 1) in
        while !offset >= 0 && !offset < length haystack do
          let local_ pattern_offset = ref (length t.pattern - 1) in
          while
            !pattern_offset >= 0
            && Char.equal
                 (unsafe_at t.pattern !pattern_offset)
                 (unsafe_at haystack !offset)
          do
            (* Search right to left through the pattern/haystack *)
            decr pattern_offset;
            decr offset
          done;
          if !pattern_offset < 0
          then (
            f (!offset + 1);
            (* FIXME: there is a jump we can do here, I'm just not sure what it is. *)
            (* Advance by one if we have a successful match *)
            offset := !offset + length t.pattern + 1)
          else
            offset
            := !offset
               + max
                   (Iarray.unsafe_get
                      t.bad_character
                      (Char.to_int (unsafe_at haystack !offset)))
                   (Iarray.unsafe_get t.bad_suffix !pattern_offset)
        done)
    ;;

    let index t haystack =
      let result =
        With_return.with_return (fun { return } ->
          indexes t haystack ~f:return;
          -1)
      in
      if result = -1 then None else exclave_ Some result
    ;;
  end

  module Search_pattern = Boyer_moore
end

module Bytes = struct
  include Make (struct
      include Bytes

      let sub_string t ~pos ~len = Bytes.sub t ~pos ~len |> to_string

      let unsafe_to_string t =
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t
      ;;
    end)

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Bytes.unsafe_blit
      ~src:src.bytes
      ~src_pos:(src.pos + src_pos)
      ~dst:dst.bytes
      ~dst_pos:(dst.pos + dst_pos)
      ~len
  ;;

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    if src_pos < 0
       || len < 0
       || src_pos + len > src.len
       || dst_pos < 0
       || dst_pos + len > dst.len
    then failwith "index out of bounds"
    else unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;
end

module String = Make (struct
    include String

    let sub_string = sub
    let unsafe_to_string t = t
  end)

include String
