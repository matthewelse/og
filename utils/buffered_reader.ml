open! Core

(* Adapted from [Eio.Buf_read]. *)

exception Buffer_limit_exceeded

type t =
  { mutable buf : Bytes.t
  ; mutable pos : int
  ; mutable len : int
  ; mutable fd : Core_unix.File_descr.t option (* None if we've seen eof *)
  ; mutable consumed : int (* Total bytes consumed so far *)
  ; max_size : int
  }

let capacity t = Bytes.length t.buf

let of_fd ?initial_size ~max_size fd =
  if max_size <= 0 then failwithf "Max size %d should be positive!" max_size ();
  let initial_size = Option.value initial_size ~default:(min 4096 max_size) in
  let buf = Bytes.create initial_size in
  { buf; pos = 0; len = 0; fd = Some fd; max_size; consumed = 0 }
;;

let peek t = exclave_ Slice.Bytes.create_local ~pos:t.pos ~len:t.len t.buf

let peek' t =
  let buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t.buf in
  exclave_ Slice.create_local ~pos:t.pos ~len:t.len buf
;;

let[@cold] consume_err t n =
  failwithf "Can't consume %d bytes of a %d byte buffer!" n t.len ()
;;

let[@inline] consume t n =
  if n < 0 || n > t.len then consume_err t n;
  t.pos <- t.pos + n;
  t.len <- t.len - n;
  t.consumed <- t.consumed + n
;;

let buffered_bytes t = t.len

let ensure_slow_path t n =
  assert (n >= 0);
  if n > t.max_size then raise Buffer_limit_exceeded;
  (* We don't have enough data yet, so we'll need to do a read. *)
  match t.fd with
  | None -> raise End_of_file
  | Some fd ->
    (* If the buffer is empty, we might as well use all of it: *)
    if t.len = 0 then t.pos <- 0;
    let () =
      let cap = capacity t in
      if n > cap
      then (
        (* [n] bytes won't fit. We need to resize the buffer. *)
        let new_size = max n (min t.max_size (cap * 2)) in
        let new_buf = Bytes.create new_size in
        Slice.Bytes.unsafe_blit
          ~src:(peek t)
          ~src_pos:0
          ~dst:(Slice.Bytes.create_local new_buf)
          ~dst_pos:0
          ~len:t.len;
        t.pos <- 0;
        t.buf <- new_buf)
      else if t.pos + n > cap
      then (
        (* [n] bytes will fit in the existing buffer, but we need to compact it first. *)
        Slice.Bytes.unsafe_blit
          ~src:(peek t)
          ~src_pos:0
          ~dst:(Slice.Bytes.create_local t.buf)
          ~dst_pos:0
          ~len:t.len;
        t.pos <- 0)
    in
    (try
       while t.len < n do
         let free_space = Slice.Bytes.create_local t.buf ~pos:(t.pos + t.len) in
         assert (t.len + Slice.Bytes.length free_space >= n);
         let got =
           Core_unix.read
             fd
             ~pos:(Slice.Bytes.Expert.pos free_space)
             ~len:(Slice.Bytes.Expert.len free_space)
             ~buf:(Slice.Bytes.Expert.bytes free_space)
         in
         if got = 0 then raise End_of_file;
         t.len <- t.len + got
       done;
       assert (buffered_bytes t >= n)
     with
     | End_of_file ->
       t.fd <- None;
       raise End_of_file)
;;

let ensure t n = if t.len < n then ensure_slow_path t n
let unsafe_get t i = Bytes.unsafe_get t.buf (t.pos + i)

let take_all t f =
  try
    while true do
      ensure t (t.len + 1)
    done;
    assert false
  with
  | End_of_file ->
    let result = f (peek' t) in
    consume t t.len;
    result
;;

let line t ~f =
  (* Return the index of the first '\n', reading more data as needed. *)
  let rec aux t i =
    let slice = Slice.Bytes.unsafe_slice (peek t) ~pos:i ~len:(t.len - i) in
    match Slice.Bytes.memchr slice '\n' with
    | None ->
      ensure t (t.len + 1);
      aux t i
    | Some pos -> pos
  in
  match aux t 0 with
  | exception End_of_file when t.len > 0 -> take_all t f
  | nl ->
    let len = if nl > 0 && Char.equal (unsafe_get t (nl - 1)) '\r' then nl - 1 else nl in
    let result =
      let buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t.buf in
      f (Slice.unsafe_create_local buf ~pos:t.pos ~len)
    in
    consume t (nl + 1);
    result
;;

let stdin ?initial_size ~max_size () = of_fd ?initial_size ~max_size Core_unix.stdin
