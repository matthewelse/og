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
