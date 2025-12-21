open! Core
open! Unboxed
open! Import

let%expect_test "test glob conversion into regexes" =
  let glob_file = {|.*
*.a
# allow .clang-format
!.clang-format
child_*/
child/*.txt|} in
  let results =
    String.split_lines glob_file
    |> List.map ~f:(fun line -> Og.For_testing.Glob.parse_string line)
  in
  print_s [%message (results : (Og.Regex.Rule.t, int * string) result list)];
  [%expect
    {|
    (results (
      (Ok (Seq ((String .) (Rep1 (Neg_group (/))))))
      (Ok (Seq ((Rep1 (Neg_group (/))) (String .a))))
      (Ok (Seq ()))
      (Ok (String .clang-format))
      (Ok (Seq ((String child_) (Rep1 (Neg_group (/))) (String /))))
      (Ok (Seq ((String child/) (Rep1 (Neg_group (/))) (String .txt))))))
    |}]
;;
