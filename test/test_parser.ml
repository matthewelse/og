open! Core

let%expect_test "regular expression parsing" =
  (* https://regex101.com/ is good for checking these *)
  let test scenario =
    let re = Og.Regex.of_string scenario |> Or_error.ok_exn in
    print_s [%sexp (re : Og.Regex.t)]
  in
  test "abc";
  [%expect {| (String abc) |}];
  test "a|bc";
  [%expect {| (Or (Seq ((String a))) (Seq ((String bc)))) |}];
  test "[abc]";
  [%expect {| (Group (a b c)) |}];
  test "[^abc]";
  [%expect {| (Neg_group (a b c)) |}];
  test "\\d";
  [%expect {| (Class Numeric) |}];
  test "\\w";
  [%expect {| (Class Alphanumeric) |}];
  test "a?bc";
  [%expect {| (Seq ((Opt (String a)) (String bc))) |}];
  test "a+bc";
  [%expect {| (Seq ((Rep1 (String a)) (String bc))) |}];
  test "(abc)+";
  [%expect {| (Rep1 (String abc)) |}];
  test "^abc$";
  [%expect {| (Seq (Start_of_line (String abc) End_of_line)) |}]
;;
