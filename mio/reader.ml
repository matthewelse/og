open! Core
open Unboxed

type t =
  | File of Fd.t
  | Buffer of
      { buffer : Bigstring.t
      ; mutable offset : i64
      }

let of_fd fd = File fd
let of_string s = Buffer { buffer = Bigstring.of_string s; offset = #0L }

external unsafe_iptr_of_bigstring
  :  Base_bigstring.t
  -> pos:i64
  -> (nativeint#[@unboxed])
  @@ portable
  = "caml_native_pointer_of_bigstring_bytecode" "caml_native_pointer_of_bigstring"
[@@noalloc] [@@no_effects] [@@no_coeffects]

module Read_result = struct
  type t =
    | Ok of I64.t
    | Eof
    | Error of I32.t
  [@@deriving sexp_of ~stackify]
end

external errno : unit -> i32 @@ portable = "mio_unreachable" "mio_errno" [@@noalloc]

let read t slice : Read_result.t @ local =
  let open I64.O in
  match t with
  | File fd ->
    let bytes_read =
      Fd.unsafe_read_assume_fd_is_nonblocking
        fd
        (unsafe_iptr_of_bigstring
           (Slice.Expert.bytes slice)
           ~pos:(Slice.Expert.pos slice))
        ~len:(Slice.Expert.len slice)
    in
    exclave_
    if bytes_read = #0L
    then Eof
    else if bytes_read > #0L
    then stack_ Ok bytes_read
    else stack_ Error (errno ())
  | Buffer buf ->
    let remaining_bytes = I64.of_int (Bigstring.length buf.buffer) - buf.offset in
    let bytes_to_read = I64.min (Slice.length slice) remaining_bytes in
    [%debug_assert bytes_to_read >= #0L];
    if bytes_to_read = #0L
    then Eof
    else (
      Bigstring.blit
        ~src:buf.buffer
        ~src_pos:(I64.to_int_trunc buf.offset)
        ~dst:(Slice.Expert.bytes slice)
        ~dst_pos:(I64.to_int_trunc (Slice.Expert.pos slice))
        ~len:(I64.to_int_trunc bytes_to_read);
      buf.offset <- buf.offset + bytes_to_read;
      exclave_ stack_ Ok bytes_to_read)
;;
