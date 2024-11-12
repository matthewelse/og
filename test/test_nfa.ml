open! Core

let%expect_test "test conversion to nfa" =
  let test scenario =
    let re = Og.Regex.of_string scenario |> Or_error.ok_exn in
    let nfa = Og.Regex.compile re in
    print_s [%sexp (nfa : Og.Regex.Compiled.t)]
  in
  test "abc";
  [%expect
    {|
    ((nodes
      (((((Literal ((pattern ((bytes abc) (pos 0) (len 3))) (offsets <opaque>))))
         1))))
     (accepting_state 1))
    |}];
  test "a|bc";
  [%expect
    {|
    ((nodes
      (((((Literal ((pattern ((bytes a) (pos 0) (len 1))) (offsets <opaque>))))
         1)
        (((Literal ((pattern ((bytes bc) (pos 0) (len 2))) (offsets <opaque>))))
         2))
       () ((() 1))))
     (accepting_state 1))
    |}];
  test "[abc]";
  [%expect {| ((nodes (((((One_of (a b c))) 1)))) (accepting_state 1)) |}];
  test "[^abc]";
  [%expect {| ((nodes (((((Not_one_of (a b c))) 1)))) (accepting_state 1)) |}];
  test "\\d";
  [%expect {| ((nodes (((((Class Numeric)) 1)))) (accepting_state 1)) |}];
  test "\\w";
  [%expect {| ((nodes (((((Class Alphanumeric)) 1)))) (accepting_state 1)) |}];
  test "a?bc";
  [%expect
    {|
    ((nodes
      (((((Literal ((pattern ((bytes a) (pos 0) (len 1))) (offsets <opaque>))))
         1)
        (() 1))
       ((((Literal ((pattern ((bytes bc) (pos 0) (len 2))) (offsets <opaque>))))
         2))))
     (accepting_state 2))
    |}];
  test "a+bc";
  [%expect
    {|
    ((nodes
      (((((Literal ((pattern ((bytes a) (pos 0) (len 1))) (offsets <opaque>))))
         1))
       ((() 0)
        (((Literal ((pattern ((bytes bc) (pos 0) (len 2))) (offsets <opaque>))))
         2))))
     (accepting_state 2))
    |}];
  test "(abc)+";
  [%expect
    {|
    ((nodes
      (((((Literal ((pattern ((bytes abc) (pos 0) (len 3))) (offsets <opaque>))))
         1))
       ((() 0))))
     (accepting_state 1))
    |}];
  test "^abc$";
  [%expect
    {|
    ((nodes
      ((((Start_of_line) 1))
       ((((Literal ((pattern ((bytes abc) (pos 0) (len 3))) (offsets <opaque>))))
         2))
       (((End_of_line) 3))))
     (accepting_state 3))
    |}]
;;
