open! Core
open! Unboxed
open! Import

let create_buffer ?(length = 100) () =
  let buf = Bigstring.create length in
  Bigstring.memset buf ~pos:0 ~len:length 'Z';
  Slice.create buf ~pos:#0L ~len:(I64.of_int length)
;;

let print_result result =
  Core.print_s ((Mio.Reader.Read_result.sexp_of_t [@alloc stack]) result) [@nontail]
;;

let%expect_test "empty reader" =
  let slice = create_buffer () in
  let reader = Mio.Reader.of_string "" in
  Mio.Reader.read reader slice |> print_result;
  [%expect {| Eof |}]
;;

let%expect_test "read smaller of buffer size and requested size" =
  let slice = create_buffer () in
  let reader = Mio.Reader.of_string "AAA" in
  Mio.Reader.read reader slice |> print_result;
  [%expect {| (Ok 3) |}];
  let slice = create_buffer ~length:1 () in
  let reader = Mio.Reader.of_string "AAA" in
  Mio.Reader.read reader slice |> print_result;
  [%expect {| (Ok 1) |}]
;;

let%expect_test "read chunks of the file" =
  let slice = create_buffer ~length:3 () in
  let reader = Mio.Reader.of_string "ABCDEFGHIJKLMN" in
  while
    match
      let result = Mio.Reader.read reader slice in
      print_result result;
      result
    with
    | Ok bytes_read ->
      let slice = Slice.slice_exn slice ~pos:#0L ~len:bytes_read in
      Slice.to_string slice |> print_endline;
      true
    | Eof -> false
    | Error errno -> raise_s [%message "unexpected error" (errno : i32)]
  do
    ()
  done;
  [%expect
    {|
    (Ok 3)
    ABC
    (Ok 3)
    DEF
    (Ok 3)
    GHI
    (Ok 3)
    JKL
    (Ok 2)
    MN
    Eof
    |}]
;;
