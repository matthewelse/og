open! Core
module Slice = Og.Slice

let%expect_test "string slices" =
  Quickcheck.test [%quickcheck.generator: string] ~sexp_of:[%sexp_of: string] ~f:(fun s ->
    for pos = 0 to String.length s - 1 do
      assert (String.equal (String.subo ~pos s) (Slice.create ~pos s |> Slice.to_string))
    done)
;;

let%expect_test "test string matching" =
  let pat = Slice.Search_pattern.create (Slice.create ~len:3 "asdf") in
  let result =
    Slice.Search_pattern.index pat (Slice.create "asdf") |> [%globalize: int option]
  in
  print_s [%message (result : int option)];
  [%expect {| (result (0)) |}]
;;

let%expect_test "test memchr" =
  let test ?offset input chr =
    let pos = Option.value ~default:0 offset in
    let slice =
      Slice.create input |> Slice.slice_exn ~pos ~len:(String.length input - pos)
    in
    print_endline [%string "slice: %{Slice.to_string slice}"];
    Slice.memchr slice chr
    |> [%globalize: int option]
    |> [%sexp_of: int option]
    |> print_s
  in
  test "asdf\r\n" '\n';
  [%expect {|
    slice: asdf

    (5)
    |}];
  test "asdf\r\n" 'e';
  [%expect {|
    slice: asdf

    ()
    |}];
  test "" 'e';
  [%expect {|
    slice:
    ()
    |}];
  test "aaaa" 'a';
  [%expect {|
    slice: aaaa
    (0)
    |}];
  test ~offset:2 "asdf\r\n" '\n';
  [%expect {|
    slice: df

    (3)
    |}];
  test ~offset:3 "asdf\r\n" 'e';
  [%expect {|
    slice: f

    ()
    |}];
  test ~offset:2 "aaaa" 'a';
  [%expect {|
    slice: aa
    (0)
    |}]
;;

let%expect_test "test string matching (string length 0)" =
  let pat = Slice.Search_pattern.create (Slice.create "") in
  let prev_index = ref (-1) in
  Slice.Search_pattern.indexes pat (Slice.create "asdf") ~f:(fun index ->
    if !prev_index = index then failwith "repeated an index.";
    prev_index := index;
    print_s [%message (index : int)]);
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "repeated an index.")
  Raised at Stdlib.failwith in file "stdlib.ml" (inlined), line 35, characters 17-33
  Called from Test_og__Test_slice.(fun) in file "test/test_slice.ml", line 77, characters 32-61
  Called from Og__Slice.Make.Search_pattern.indexes_from in file "lib/slice.ml" (inlined), line 172, characters 48-56
  Called from Og__Slice.Make.Search_pattern.indexes in file "lib/slice.ml", line 182, characters 32-68
  Called from Test_og__Test_slice.(fun) in file "test/test_slice.ml", lines 76-79, characters 2-199
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  (index 0)
  |}]
;;

let%expect_test "quickcheck" =
  let test_slice ~needle ~haystack =
    let needle = Og.Slice.Search_pattern.create (Slice.create needle) in
    [%globalize: int option]
      (Og.Slice.Search_pattern.index needle (Slice.create_local haystack)) [@nontail]
  in
  let test_string ~needle ~haystack =
    let needle = String.Search_pattern.create needle in
    String.Search_pattern.index needle ~in_:haystack |> [%globalize: int option]
  in
  Quickcheck.test
    ~sexp_of:[%sexp_of: string * string]
    [%quickcheck.generator: string * string]
    ~f:(fun (needle, salt) ->
      (* Try and exercise the algorithm as much as possible -- test vs. the salt
         on its own, and various cases that should ~always result in a match. *)
      assert (
        [%compare.equal: int option]
          (test_slice ~needle ~haystack:salt)
          (test_string ~needle ~haystack:salt));
      assert (
        [%compare.equal: int option]
          (test_slice ~needle ~haystack:(needle ^ salt))
          (test_string ~needle ~haystack:(needle ^ salt)));
      assert (
        [%compare.equal: int option]
          (test_slice ~needle ~haystack:(salt ^ needle))
          (test_string ~needle ~haystack:(salt ^ needle)));
      assert (
        [%compare.equal: int option]
          (test_slice ~needle ~haystack:(salt ^ needle ^ salt))
          (test_string ~needle ~haystack:(salt ^ needle ^ salt))))
;;
