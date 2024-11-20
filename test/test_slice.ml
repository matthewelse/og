open! Core
open! Import
module Slice = Og_utils.Slice

let%expect_test "string slices" =
  Quickcheck.test [%quickcheck.generator: string] ~sexp_of:[%sexp_of: string] ~f:(fun s ->
    for pos = 0 to String.length s - 1 do
      assert (
        String.equal
          (String.subo ~pos s)
          (Slice.create s ~pos:(I64.of_int pos) ~len:(String.length s - pos |> I64.of_int)
           |> Slice.to_string))
    done)
;;

let%expect_test "test string matching" =
  let pat = Slice.Search_pattern.create (Slice.create ~pos:#0L ~len:#3L "asdf") in
  let result =
    Slice.Search_pattern.index pat (Slice.of_string "asdf") |> I64.Option.box
  in
  print_s [%message (result : int64 option)];
  [%expect {| (result (0)) |}]
;;

let%expect_test "test memchr" =
  let test ?offset input chr =
    let pos = Option.value ~default:0 offset in
    let slice =
      Slice.of_string input
      |> Slice.slice_exn
           ~pos:(I64.of_int pos)
           ~len:(String.length input - pos |> I64.of_int)
    in
    print_endline [%string "slice: %{Slice.to_string slice}"];
    Slice.memchr slice chr |> I64.Option.box |> [%sexp_of: int64 option] |> print_s
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
  let pat = Slice.Search_pattern.create (Slice.of_string "") in
  let prev_index = ref (-1) in
  Slice.Search_pattern.indexes pat (Slice.of_string "asdf") ~f:(fun index ->
    if !prev_index = I64.to_int_trunc index then failwith "repeated an index.";
    prev_index := I64.to_int_trunc index;
    print_s [%message "" ~index:(I64.to_int_trunc index : int)]);
  [%expect
    {|
    (index 0)
    (index 1)
    (index 2)
    (index 3)
    (index 4)
    |}];
  (* Core's implementation for comparison. *)
  let pat = String.Search_pattern.create "" in
  let prev_index = ref (-1) in
  String.Search_pattern.index_all pat ~in_:"asdf" ~may_overlap:true
  |> List.iter ~f:(fun index ->
    if !prev_index = index then failwith "repeated an index.";
    prev_index := index;
    print_s [%message (index : int)]);
  [%expect
    {|
    (index 0)
    (index 1)
    (index 2)
    (index 3)
    (index 4)
    |}]
;;

let%expect_test "quickcheck" =
  let test_slice ~needle ~haystack =
    let needle = Slice.Search_pattern.create (Slice.of_string needle) in
    (I64.Option.box (Slice.Search_pattern.index needle (Slice.of_string haystack))
     |> Option.map ~f:Int64.to_int_exn) [@nontail]
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
