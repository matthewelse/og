open! Core

let%expect_test "regular expression parsing" =
  (* https://regex101.com/ is good for checking these *)
  let test scenario =
    let re = Og.Regex.of_string scenario |> Or_error.ok_exn in
    print_s [%sexp (re : Og.Regex.t)]
  in
  test "abc";
  [%expect {| ((re (String abc)) (flags ())) |}];
  test "a|bc";
  [%expect {| ((re (Or (Seq ((String a))) (Seq ((String bc))))) (flags ())) |}];
  test "[abc]";
  [%expect {| ((re (Group (a b c))) (flags ())) |}];
  test "[^abc]";
  [%expect {| ((re (Neg_group (a b c))) (flags ())) |}];
  test "\\d";
  [%expect {| ((re (Class Numeric)) (flags ())) |}];
  test "\\w";
  [%expect {| ((re (Class Alphanumeric)) (flags ())) |}];
  test "a?bc";
  [%expect {| ((re (Seq ((Opt (String a)) (String bc)))) (flags ())) |}];
  test "a+bc";
  [%expect {| ((re (Seq ((Rep1 (String a)) (String bc)))) (flags ())) |}];
  test "(abc)+";
  [%expect {| ((re (Rep1 (String abc))) (flags ())) |}];
  test "^abc$";
  [%expect {| ((re (String abc)) (flags (Require_sol Require_eol))) |}]
;;
