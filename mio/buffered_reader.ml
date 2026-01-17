open! Core
open Unboxed

(* Adapted from [Eio.Buf_read]. *)

exception Buffer_limit_exceeded

type t =
  { mutable buf : Bigstring.t
  ; reader : Reader.t
  ; mutable pos : int64#
  ; mutable len : int64#
  ; mutable consumed : int64# (* Total bytes consumed so far *)
  ; max_size : int64#
  }

let capacity t = Bigstring.length t.buf |> I64.of_int

let of_fd ?initial_size ~max_size fd =
  if max_size <= 0 then failwithf "Max size %d should be positive!" max_size ();
  let initial_size =
    Option.value initial_size ~default:(Int.min 4096 max_size) |> I64.of_int
  in
  let buf = Bigstring.create (I64.to_int_exn initial_size) in
  { buf
  ; pos = #0L
  ; len = #0L
  ; reader = Reader.of_fd fd
  ; max_size = I64.of_int max_size
  ; consumed = #0L
  }
;;

let of_string ?initial_size ~max_size str =
  if max_size <= 0 then failwithf "Max size %d should be positive!" max_size ();
  let initial_size =
    Option.value initial_size ~default:(Int.min 4096 max_size) |> I64.of_int
  in
  let buf = Bigstring.create (I64.to_int_exn initial_size) in
  { buf
  ; pos = #0L
  ; len = #0L
  ; reader = Reader.of_string str
  ; max_size = I64.of_int max_size
  ; consumed = #0L
  }
;;

let peek t = exclave_ Slice.create_local ~pos:t.pos ~len:t.len t.buf

let[@cold] consume_err t n =
  failwithf
    "Can't consume %d bytes of a %d byte buffer!"
    (I64.to_int_exn n)
    (I64.to_int_exn t.len)
    ()
;;

let[@inline] consume t n =
  let open I64.O in
  if n < #0L || n > t.len then consume_err t n;
  t.pos <- t.pos + n;
  t.len <- t.len - n;
  t.consumed <- t.consumed + n
;;

let buffered_bytes t = t.len

external mio_explain_error
  :  unit
  -> unit
  @@ portable
  = "mio_unreachable" "mio_explain_error"
[@@noalloc]

let ensure_slow_path t n =
  let open I64.O in
  assert (n >= #0L);
  if n > t.max_size then raise_notrace Buffer_limit_exceeded;
  (* We don't have enough data yet, so we'll need to do a read. *)
  (* If the buffer is empty, we might as well use all of it: *)
  if t.len = #0L then t.pos <- #0L;
  let () =
    let cap = capacity t in
    if n > cap
    then (
      (* [n] bytes won't fit. We need to resize the buffer. *)
      let new_size = I64.max n (I64.min t.max_size (cap * #2L)) in
      let new_buf = Bigstring.create (I64.to_int_trunc new_size) in
      Slice.unsafe_blit
        ~src:(peek t)
        ~src_pos:0
        ~dst:(Slice.create_local new_buf ~pos:#0L ~len:new_size)
        ~dst_pos:0
        ~len:(I64.to_int_trunc t.len);
      t.pos <- #0L;
      t.buf <- new_buf)
    else if t.pos + n > cap
    then (
      (* [n] bytes will fit in the existing buffer, but we neefiled to compact it first. *)
      Slice.unsafe_blit
        ~src:(peek t)
        ~src_pos:0
        ~dst:(Slice.create_local t.buf ~pos:#0L ~len:t.len)
        ~dst_pos:0
        ~len:(I64.to_int_trunc t.len);
      t.pos <- #0L)
  in
  try
    while t.len < n do
      let free_space =
        Slice.create_local
          t.buf
          ~pos:(t.pos + t.len)
          ~len:((Bigstring.length t.buf |> I64.of_int) - t.pos - t.len)
      in
      assert (t.len + Slice.length free_space >= n);
      let got : i64 =
        match Reader.read t.reader free_space with
        | Eof ->
          (match raise_notrace End_of_file with
           | (_ : Nothing.t) -> .)
        | Error _ ->
          mio_explain_error ();
          (match failwith "todo error" with
           | (_ : Nothing.t) -> .)
        | Ok bytes_read -> bytes_read
      in
      t.len <- t.len + got
    done;
    assert (buffered_bytes t >= n)
  with
  | End_of_file -> raise_notrace End_of_file
;;

let ensure t n =
  let open I64.O in
  if t.len < n then ensure_slow_path t n
;;

let unsafe_get t i =
  let open I64.O in
  Bigstring.unsafe_get t.buf (t.pos + i |> I64.to_int_trunc)
;;

let take_all t f =
  let open I64.O in
  try
    while true do
      ensure t (t.len + #1L)
    done;
    assert false
  with
  | End_of_file ->
    let result = f (peek t) in
    consume t t.len;
    result
;;

let ensure_line t =
  let open I64.O in
  (* Return the index of the first '\n', reading more data as needed. *)
  let[@loopify] rec aux t i =
    let slice = Slice.unsafe_slice (peek t) ~pos:i ~len:(t.len - i) in
    match%optional_u.I64.Option Slice.memchr slice '\n' with
    | None ->
      ensure t (t.len + #1L);
      aux t i
    | Some pos -> pos
  in
  match aux t #0L with
  | exception End_of_file when t.len > #0L -> ()
  | nl ->
    let _len =
      if nl > #0L && Char.equal (unsafe_get t (nl - #1L)) '\r' then nl - #1L else nl
    in
    ()
;;

let line t ~f =
  let open I64.O in
  (* Return the index of the first '\n', reading more data as needed. *)
  let[@loopify] rec aux t i =
    let slice = Slice.unsafe_slice (peek t) ~pos:i ~len:(t.len - i) in
    match%optional_u.I64.Option Slice.memchr slice '\n' with
    | None ->
      ensure t (t.len + #1L);
      aux t i
    | Some pos -> pos
  in
  match aux t #0L with
  | exception End_of_file when t.len > #0L -> take_all t f
  | nl ->
    let len =
      if nl > #0L && Char.equal (unsafe_get t (nl - #1L)) '\r' then nl - #1L else nl
    in
    let result = f (Slice.unsafe_create_local t.buf ~pos:t.pos ~len) in
    consume t (nl + #1L);
    result
;;

let chunk t ~f =
  let open I64.O in
  let[@loopify] rec aux t i =
    let slice = Slice.unsafe_slice (peek t) ~pos:i ~len:(t.len - i) in
    match%optional_u.I64.Option Slice.rmemchr slice '\n' with
    | None ->
      ensure t (t.len + #1L);
      aux t i
    | Some pos -> pos
  in
  match aux t #0L with
  | exception End_of_file when t.len > #0L -> take_all t f
  | nl ->
    (* Consume up to and including the newline. *)
    let len = nl + #1L in
    let result = f (Slice.unsafe_create_local t.buf ~pos:t.pos ~len) in
    consume t (nl + #1L);
    result
;;

let stdin ?initial_size ~max_size () = of_fd ?initial_size ~max_size (Fd.stdin ())

let is_probably_binary t ~num_bytes_to_read =
  let open I64.O in
  let buf = peek t in
  let len = I64.min (Slice.length buf) num_bytes_to_read in
  [%debug_assert len >= #0L];
  let buf = Slice.unsafe_slice buf ~pos:#0L ~len in
  Slice.exists buf ~f:(fun byte -> Int.O.(Char.to_int byte >= 128)) [@nontail]
;;
