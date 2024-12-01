open! Core

let%expect_test "regular expression matching" =
  (* https://regex101.com/ is good for checking these *)
  List.iter Og.Implementation.all ~f:(fun impl ->
    let test scenario ~ok_inputs ~error_inputs =
      let re = Og.Regex.of_string scenario |> Or_error.ok_exn |> Og.Regex.compile ~impl in
      let test_str re str =
        let slice = Og_utils.Slice.of_string str in
        Og.Regex.Compiled.matches re slice
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
    ([%expect
       {| |}];
     test "a|bc" ~ok_inputs:[ "a"; "bc"; "abc"; "ab" ] ~error_inputs:[ "b"; "c"; "" ];
     [%expect
       {| |}];
     test
       "[abc]"
       ~ok_inputs:[ "a"; "b"; "c"; "aaabbbccc"; "abc" ]
       ~error_inputs:[ ""; "d" ];
     [%expect
       {| |}];
     test "[^abc]" ~ok_inputs:[ "d"; "da" ] ~error_inputs:[ "a"; "b"; "c"; "abc" ];
     [%expect
       {| |}];
     test "\\d" ~ok_inputs:[ "012345678"; "1"; "9"; "a9" ] ~error_inputs:[ "a" ];
     [%expect
       {| |}];
     test "\\w" ~ok_inputs:[ "a"; "0"; "a." ] ~error_inputs:[ "."; "\\" ];
     [%expect
       {| |}];
     test "a?bc" ~ok_inputs:[ "abc"; "bc"; "aabc" ] ~error_inputs:[ "a"; "b"; "c" ];
     [%expect
       {| |}];
     test "a+bc" ~ok_inputs:[ "aabc" ] ~error_inputs:[ "bc" ];
     [%expect
       {| |}];
     test "(abc)+" ~ok_inputs:[ "abc"; "abcabc" ] ~error_inputs:[ ""; "a"; "ab" ];
     [%expect
       {| |}];
     test "^abc$" ~ok_inputs:[ "abc" ] ~error_inputs:[ ""; " abc"; "abc "; " abc " ];
     [%expect
       {| |}];
     test
       "nananax?x"
       ~ok_inputs:[ "nananax"; "nananaxx"; "nanananaxx" ]
       ~error_inputs:[ "nanana" ];
     [%expect
       {| |}];
     test
       "^#?i?n?c?l?u?d?e?"
       ~ok_inputs:
         [ ""; "include"; "#include"; "i"; "n"; "c"; "l"; "u"; "d"; "e"; "  #include" ]
       ~error_inputs:[];
     [%expect
       {| |}];
     test "^[]$" ~ok_inputs:[] ~error_inputs:[ ""; "[]"; "[] "; " []"; "a"; "][" ];
     [%expect
       {| |}]
     : unit))
;;

let%expect_test "regression test: `nfa_hybrid` bug" =
  let regex : Og.Regex.t =
    { re =
        Or
          ( String "./certs/x509_revocation_list"
          , Or (String "./certs/x509_certificate_list", String "./certs/extract-cert") )
    ; flags = Og.Flags.of_list [ Require_sol; Require_eol ]
    }
  in
  let regex = Og.Regex.compile ~impl:Og.Implementation.Nfa_hybrid regex in
  let slice = Og_utils.Slice.of_string "./certs/common.c" in
  print_s [%message (regex : Og.Regex.Compiled.t)];
  [%expect
    {|
    (regex
     ((nfa
       ((Split 1 30) (Match (.) 2) (Match (/) 3) (Match (c) 4) (Match (e) 5)
        (Match (r) 6) (Match (t) 7) (Match (s) 8) (Match (/) 9) (Match (x) 10)
        (Match (5) 11) (Match (0) 12) (Match (9) 13) (Match (_) 14)
        (Match (r) 15) (Match (e) 16) (Match (v) 17) (Match (o) 18)
        (Match (c) 19) (Match (a) 20) (Match (t) 21) (Match (i) 22)
        (Match (o) 23) (Match (n) 24) (Match (_) 25) (Match (l) 26)
        (Match (i) 27) (Match (s) 28) (Match (t) 29) (Jump 81) (Split 31 61)
        (Match (.) 32) (Match (/) 33) (Match (c) 34) (Match (e) 35)
        (Match (r) 36) (Match (t) 37) (Match (s) 38) (Match (/) 39)
        (Match (x) 40) (Match (5) 41) (Match (0) 42) (Match (9) 43)
        (Match (_) 44) (Match (c) 45) (Match (e) 46) (Match (r) 47)
        (Match (t) 48) (Match (i) 49) (Match (f) 50) (Match (i) 51)
        (Match (c) 52) (Match (a) 53) (Match (t) 54) (Match (e) 55)
        (Match (_) 56) (Match (l) 57) (Match (i) 58) (Match (s) 59)
        (Match (t) 60) (Jump 81) (Match (.) 62) (Match (/) 63) (Match (c) 64)
        (Match (e) 65) (Match (r) 66) (Match (t) 67) (Match (s) 68)
        (Match (/) 69) (Match (e) 70) (Match (x) 71) (Match (t) 72)
        (Match (r) 73) (Match (a) 74) (Match (c) 75) (Match (t) 76)
        (Match (-) 77) (Match (c) 78) (Match (e) 79) (Match (r) 80)
        (Match (t) 81) Accept))
      (cache ()) (accepting_state 81) (flags (Require_sol Require_eol))
      (look_for_constant ())))
    |}];
  Expect_test_helpers_core.require_does_not_raise (fun () ->
    Og.Regex.Compiled.matches regex slice |> (ignore : bool -> unit));
  [%expect
    {| |}]
;;
