open! Core

let%expect_test "regular expression matching" =
  (* https://regex101.com/ is good for checking these *)
  let test scenario ~ok_inputs ~error_inputs =
    let re = Og.Regex.of_string scenario |> Or_error.ok_exn |> Og.Regex.compile in
    let test_str re str =
      let slice = Og.Slice.create_local str in
      Og.Regex.Compiled.matches re slice [@nontail]
    in
    let ok_matches = List.find ok_inputs ~f:(Fn.non (test_str re)) in
    Expect_test_helpers_core.require_none sexp_of_string ok_matches;
    let error_matches = List.find error_inputs ~f:(test_str re) in
    Expect_test_helpers_core.require_none sexp_of_string error_matches
  in
  test
    "abc"
    ~ok_inputs:[ "abc"; "   abc"; "abcabc" ]
    ~error_inputs:[ "a"; "ab"; "abd"; "ab c"; "" ];
  [%expect {| |}];
  test "a|bc" ~ok_inputs:[ "a"; "bc"; "abc"; "ab" ] ~error_inputs:[ "b"; "c"; "" ];
  [%expect {| |}];
  test "[abc]" ~ok_inputs:[ "a"; "b"; "c"; "aaabbbccc"; "abc" ] ~error_inputs:[ ""; "d" ];
  [%expect {| |}];
  test "[^abc]" ~ok_inputs:[ "d"; "da" ] ~error_inputs:[ "a"; "b"; "c"; "abc" ];
  [%expect {| |}];
  test "\\d" ~ok_inputs:[ "012345678"; "1"; "9"; "a9" ] ~error_inputs:[ "a" ];
  [%expect {| |}];
  test "\\w" ~ok_inputs:[ "a"; "0"; "a." ] ~error_inputs:[ "."; "\\" ];
  [%expect {| |}];
  test "a?bc" ~ok_inputs:[ "abc"; "bc"; "aabc" ] ~error_inputs:[ "a"; "b"; "c" ];
  [%expect {| |}];
  test "a+bc" ~ok_inputs:[ "aabc" ] ~error_inputs:[ "bc" ];
  [%expect {| |}];
  test "(abc)+" ~ok_inputs:[ "abc"; "abcabc" ] ~error_inputs:[ ""; "a"; "ab" ];
  [%expect {| |}];
  test "^abc$" ~ok_inputs:[ "abc" ] ~error_inputs:[ ""; " abc"; "abc "; " abc " ];
  [%expect {| |}];
  test
    "nananax?x"
    ~ok_inputs:[ "nananax"; "nananaxx"; "nanananaxx" ]
    ~error_inputs:[ "nanana" ]
;;
