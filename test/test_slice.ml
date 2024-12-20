open! Core
open! Import
module Slice = Og_utils.Slice

let%expect_test "string slices" =
  Quickcheck.test
    [%quickcheck.generator: string]
    ~sexp_of:[%sexp_of: string]
    ~f:(fun s' ->
      let s = Bigstring.of_string s' in
      for pos = 0 to Bigstring.length s - 1 do
        assert (
          String.equal
            (String.subo ~pos s')
            (Slice.create
               s
               ~pos:(I64.of_int pos)
               ~len:(Bigstring.length s - pos |> I64.of_int)
             |> Slice.to_string))
      done)
;;

let%expect_test "test string matching" =
  let pat =
    Slice.Search_pattern.create
      (Slice.of_string "asdf" |> Slice.slice_exn ~pos:#0L ~len:#3L |> Slice.globalize)
  in
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

let%expect_test "test rmemchr" =
  let test ?offset input chr =
    let pos = Option.value ~default:0 offset in
    let slice =
      Slice.of_string input
      |> Slice.slice_exn
           ~pos:(I64.of_int pos)
           ~len:(String.length input - pos |> I64.of_int)
    in
    print_endline [%string "slice: %{Slice.to_string slice}"];
    Slice.rmemchr slice chr |> I64.Option.box |> [%sexp_of: int64 option] |> print_s
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
    (3)
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
    (1)
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

let%expect_test "quickcheck memchr" =
  Quickcheck.test
    [%quickcheck.generator: string * char]
    ~sexp_of:[%sexp_of: string * char]
    ~f:(fun (haystack, needle) ->
      let core_result = Core.String.index haystack needle |> Option.map ~f:Int64.of_int in
      let haystack = Slice.of_string haystack in
      let ocaml_result = Slice.memchr haystack needle |> I64.Option.box in
      if not ([%compare.equal: int64 option] core_result ocaml_result)
      then
        raise_s
          [%message "Mismatch" (core_result : int64 option) (ocaml_result : int64 option)])
;;

let%expect_test "quickcheck rmemchr" =
  Quickcheck.test
    [%quickcheck.generator: string * char]
    ~sexp_of:[%sexp_of: string * char]
    ~f:(fun (haystack, needle) ->
      let core_result =
        Core.String.rindex haystack needle |> Option.map ~f:Int64.of_int
      in
      let haystack = Slice.of_string haystack in
      let ocaml_result = Slice.rmemchr haystack needle |> I64.Option.box in
      if not ([%compare.equal: int64 option] core_result ocaml_result)
      then
        raise_s
          [%message "Mismatch" (core_result : int64 option) (ocaml_result : int64 option)])
;;

let%expect_test _ =
  let s = Bigstring.of_string "8IsrYsLVW40\000BO5RmuH\148" in
  let _needle = 'V' in
  let first = Bigstring.get_int64_t_le s ~pos:4 in
  print_s [%message (first : Int64.Hex.t)];
  [%expect {| (first 0x303457564c7359) |}];
  let open Int64.O in
  let mask_lo = 0x0101_0101_0101_0101L in
  let mask_hi = 0x8080_8080_8080_8080L in
  let inter = first lxor 0x5656_5656_5656_5656L in
  print_s [%message (inter : Int64.Hex.t)];
  [%expect {| (inter 0x56666201001a250f) |}];
  let a = inter - mask_lo in
  let b = lnot inter land mask_hi in
  let res = a land b in
  print_s [%message (a : Int64.Hex.t) (b : Int64.Hex.t) (res : Int64.Hex.t)];
  [%expect {| ((a 0x556560ffff19240e) (b -0x7f7f7f7f7f7f7f80) (res 0x8080000000)) |}];
  let clz = Int64.clz res in
  let ctz = Int64.ctz res in
  print_s [%message (clz : int) (ctz : int)];
  [%expect {| ((clz 24) (ctz 31)) |}];
  let open Int.O in
  print_s [%message (3 + 8 - (clz / 8) : int)];
  [%expect {| ("(3 + 8) - (clz / 8)" 8) |}]
;;
