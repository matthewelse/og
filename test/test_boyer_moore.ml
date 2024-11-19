open! Core

let pattern = "EXAMPLE"
let haystack = "HERE IS A SIMPLE EXAMPLE"

let compute_bad_character_table pattern =
  let bad_character_table = Array.create ~len:256 (String.length pattern) in
  for i = 0 to String.length pattern - 1 do
    bad_character_table.(Char.to_int pattern.[i]) <- String.length pattern - 1 - i
  done;
  bad_character_table
;;

let%expect_test "quickcheck [compute_bad_character_table]" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: string]
    [%quickcheck.generator: string]
    ~f:(fun pattern ->
      let table = compute_bad_character_table pattern in
      (* from the paper (1-indexed)

         "if char does not occur in pat, then patlen, else patlen - j where j is
          the maximum integer such that pat[j] = char"

         (0-indexed)

         "if char does not occur in pat, then patlen, else patlen - j - 1 where
          j is the maximum integer such that pat[j] = char"
      *)
      let chars = Char.Set.of_list (String.to_list pattern) in
      Array.iteri table ~f:(fun c shift ->
        let c = Char.of_int_exn c in
        let expected =
          if not (Set.mem chars c)
          then String.length pattern
          else (
            let j =
              String.rfindi pattern ~f:(fun _ c' -> Char.equal c' c) |> Option.value_exn
            in
            String.length pattern - j - 1)
        in
        assert (shift = expected)))
;;

let print_bad_character_table patlen table =
  Array.filter_mapi table ~f:(fun c entry ->
    if entry = patlen then None else Some (Char.of_int_exn c, entry))
  |> [%sexp_of: (char * int) array]
  |> print_s
;;

let%expect_test _ =
  print_bad_character_table (String.length pattern) (compute_bad_character_table pattern);
  [%expect {| ((A 4) (E 0) (L 1) (M 3) (P 2) (X 5)) |}]
;;

let at s i = if i < 0 || i >= String.length s then None else Some s.[i]

let%expect_test "compute delta2" =
  let pattern = "ABYXCDEYX" in
  let delta_2 = Array.create ~len:(String.length pattern) 0 in
  for suffix_length = 0 to String.length pattern - 1 do
    With_return.with_return (fun { return } ->
      for offset = 1 to String.length pattern do
        (* TODO: make this comprehensible *)
        let suffix = String.suffix pattern suffix_length in
        let prefix = String.prefix pattern (String.length pattern - offset) in
        let prefix_suffix = String.suffix prefix suffix_length in
        if String.is_suffix ~suffix:prefix_suffix suffix
           && not
                ([%compare.equal: char option]
                   (at pattern (String.length pattern - 1 - suffix_length))
                   (at pattern (String.length pattern - 1 - suffix_length - offset)))
        then (
          delta_2.(String.length pattern - 1 - suffix_length) <- suffix_length + offset;
          return ())
      done);
    ()
  done;
  print_s [%message (delta_2 : int array)];
  [%expect {| (delta_2 (17 16 15 14 13 12 7 10 1)) |}]
;;

let compute_bad_suffix_table pattern =
  let delta_2 = Array.create ~len:(String.length pattern) 0 in
  for suffix_length = 0 to String.length pattern - 1 do
    With_return.with_return (fun { return } ->
      for offset = 1 to String.length pattern do
        (* TODO: make this comprehensible *)
        let suffix = String.suffix pattern suffix_length in
        let prefix = String.prefix pattern (String.length pattern - offset) in
        let prefix_suffix = String.suffix prefix suffix_length in
        if String.is_suffix ~suffix:prefix_suffix suffix
           && not
                ([%compare.equal: char option]
                   (at pattern (String.length pattern - 1 - suffix_length))
                   (at pattern (String.length pattern - 1 - suffix_length - offset)))
        then (
          delta_2.(String.length pattern - 1 - suffix_length) <- suffix_length + offset;
          return ())
      done)
  done;
  delta_2
;;

let%expect_test "compute delta2" =
  let pattern = "mississi" in
  let delta_2 = compute_bad_suffix_table pattern in
  print_s [%message (delta_2 : int array)];
  [%expect {| (delta_2 (15 14 13 7 11 10 7 1)) |}]
;;

let does_match pattern haystack =
  let bad_character_table = compute_bad_character_table pattern in
  let bad_suffix_table = compute_bad_suffix_table pattern in
  With_return.with_return (fun { return } ->
    let local_ offset = ref (String.length pattern - 1) in
    while !offset >= 0 && !offset < String.length haystack do
      let local_ pattern_offset = ref (String.length pattern - 1) in
      while
        !pattern_offset >= 0 && Char.equal pattern.[!pattern_offset] haystack.[!offset]
      do
        (* Search right to left through the pattern/haystack *)
        decr pattern_offset;
        decr offset
      done;
      if !pattern_offset < 0 then return true;
      offset
      := max
           bad_character_table.(Char.to_int haystack.[!offset])
           bad_suffix_table.(!pattern_offset)
    done;
    false)
;;

let%expect_test "asdf" =
  let test pattern haystack =
    let result = does_match pattern haystack in
    print_s [%message (result : bool)]
  in
  test "ABC" "ABCXXXABX";
  [%expect {| (result true) |}]
;;
