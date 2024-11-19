open! Core

let%expect_test "test conversion to nfa" =
  let test scenario =
    let re = Og.Regex.of_string scenario |> Or_error.ok_exn in
    List.iter Og.Implementation.all ~f:(fun impl ->
      let nfa = Og.Regex.compile ~impl re in
      print_s [%message (impl : Og.Implementation.t) (nfa : Og.Regex.Compiled.t)])
  in
  test "abc";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes abc) (pos 0) (len 3))))))
           1))))
       (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Match (a) 1) (Match (b) 2) (Match (c) 3) Accept)) (cache ())
       (accepting_state 3) (flags ())
       (look_for_constant
        (((bad_character <opaque>) (bad_suffix <opaque>)
          (pattern ((bytes abc) (pos 0) (len 3)))))))))
    |}];
  test "a|bc";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes a) (pos 0) (len 1))))))
           1)
          (((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes bc) (pos 0) (len 2))))))
           2))
         () ((() 1))))
       (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa
        ((Split 1 3) (Match (a) 2) (Jump 5) (Match (b) 4) (Match (c) 5) Accept))
       (cache ()) (accepting_state 5) (flags ()) (look_for_constant ()))))
    |}];
  test "[abc]";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa ((nodes (((((One_of (a b c))) 1)))) (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Match (a b c) 1) Accept)) (cache ()) (accepting_state 1) (flags ())
       (look_for_constant ()))))
    |}];
  test "[^abc]";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes (((((Not_one_of (a b c))) 1)))) (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa
        ((Match
          ("\000" "\001" "\002" "\003" "\004" "\005" "\006" "\007" "\b" "\t" "\n"
           "\011" "\012" "\r" "\014" "\015" "\016" "\017" "\018" "\019" "\020"
           "\021" "\022" "\023" "\024" "\025" "\026" "\027" "\028" "\029" "\030"
           "\031" " " ! "\"" # $ % & ' "(" ")" * + , - . / 0 1 2 3 4 5 6 7 8 9 :
           ";" < = > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [
           "\\" ] ^ _ ` d e f g h i j k l m n o p q r s t u v w x y z { | } ~
           "\127" "\128" "\129" "\130" "\131" "\132" "\133" "\134" "\135" "\136"
           "\137" "\138" "\139" "\140" "\141" "\142" "\143" "\144" "\145" "\146"
           "\147" "\148" "\149" "\150" "\151" "\152" "\153" "\154" "\155" "\156"
           "\157" "\158" "\159" "\160" "\161" "\162" "\163" "\164" "\165" "\166"
           "\167" "\168" "\169" "\170" "\171" "\172" "\173" "\174" "\175" "\176"
           "\177" "\178" "\179" "\180" "\181" "\182" "\183" "\184" "\185" "\186"
           "\187" "\188" "\189" "\190" "\191" "\192" "\193" "\194" "\195" "\196"
           "\197" "\198" "\199" "\200" "\201" "\202" "\203" "\204" "\205" "\206"
           "\207" "\208" "\209" "\210" "\211" "\212" "\213" "\214" "\215" "\216"
           "\217" "\218" "\219" "\220" "\221" "\222" "\223" "\224" "\225" "\226"
           "\227" "\228" "\229" "\230" "\231" "\232" "\233" "\234" "\235" "\236"
           "\237" "\238" "\239" "\240" "\241" "\242" "\243" "\244" "\245" "\246"
           "\247" "\248" "\249" "\250" "\251" "\252" "\253" "\254" "\255")
          1)
         Accept))
       (cache ()) (accepting_state 1) (flags ()) (look_for_constant ()))))
    |}];
  test "\\d";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa ((nodes (((((Class Numeric)) 1)))) (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Match (0 1 2 3 4 5 6 7 8 9) 1) Accept)) (cache ())
       (accepting_state 1) (flags ()) (look_for_constant ()))))
    |}];
  test "\\w";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes (((((Class Alphanumeric)) 1)))) (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa
        ((Match
          (0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y
           Z a b c d e f g h i j k l m n o p q r s t u v w x y z)
          1)
         Accept))
       (cache ()) (accepting_state 1) (flags ()) (look_for_constant ()))))
    |}];
  test "a?bc";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes a) (pos 0) (len 1))))))
           1)
          (() 1))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes bc) (pos 0) (len 2))))))
           2))))
       (accepting_state 2) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Split 1 2) (Match (a) 2) (Match (b) 3) (Match (c) 4) Accept))
       (cache ()) (accepting_state 4) (flags ())
       (look_for_constant
        (((bad_character <opaque>) (bad_suffix <opaque>)
          (pattern ((bytes bc) (pos 0) (len 2)))))))))
    |}];
  test "a+bc";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes a) (pos 0) (len 1))))))
           1))
         ((() 0)
          (((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes bc) (pos 0) (len 2))))))
           2))))
       (accepting_state 2) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Match (a) 1) (Split 2 0) (Match (b) 3) (Match (c) 4) Accept))
       (cache ()) (accepting_state 4) (flags ())
       (look_for_constant
        (((bad_character <opaque>) (bad_suffix <opaque>)
          (pattern ((bytes bc) (pos 0) (len 2)))))))))
    |}];
  test "(abc)+";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes abc) (pos 0) (len 3))))))
           1))
         ((() 0))))
       (accepting_state 1) (flags ()))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Match (a) 1) (Match (b) 2) (Match (c) 3) (Split 4 0) Accept))
       (cache ()) (accepting_state 4) (flags ()) (look_for_constant ()))))
    |}];
  test "^abc$";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes abc) (pos 0) (len 3))))))
           1))))
       (accepting_state 1) (flags (Require_sol Require_eol)))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa ((Match (a) 1) (Match (b) 2) (Match (c) 3) Accept)) (cache ())
       (accepting_state 3) (flags (Require_sol Require_eol))
       (look_for_constant
        (((bad_character <opaque>) (bad_suffix <opaque>)
          (pattern ((bytes abc) (pos 0) (len 3)))))))))
    |}];
  test "^#?i?n?c?l?u?d?e?";
  [%expect
    {|
    ((impl Nfa_backtrack)
     (nfa
      ((nodes
        (((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes #) (pos 0) (len 1))))))
           1)
          (() 1))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes i) (pos 0) (len 1))))))
           2)
          (() 2))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes n) (pos 0) (len 1))))))
           3)
          (() 3))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes c) (pos 0) (len 1))))))
           4)
          (() 4))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes l) (pos 0) (len 1))))))
           5)
          (() 5))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes u) (pos 0) (len 1))))))
           6)
          (() 6))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes d) (pos 0) (len 1))))))
           7)
          (() 7))
         ((((Literal
             ((bad_character <opaque>) (bad_suffix <opaque>)
              (pattern ((bytes e) (pos 0) (len 1))))))
           8)
          (() 8))))
       (accepting_state 8) (flags (Require_sol)))))
    ((impl Nfa_hybrid)
     (nfa
      ((nfa
        ((Split 1 2) (Match (#) 2) (Split 3 4) (Match (i) 4) (Split 5 6)
         (Match (n) 6) (Split 7 8) (Match (c) 8) (Split 9 10) (Match (l) 10)
         (Split 11 12) (Match (u) 12) (Split 13 14) (Match (d) 14) (Split 15 16)
         (Match (e) 16) Accept))
       (cache ()) (accepting_state 16) (flags (Require_sol))
       (look_for_constant ()))))
    |}]
;;
