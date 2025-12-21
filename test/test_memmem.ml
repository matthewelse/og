open! Core
open Unboxed
open! Import

let%expect_test "basic example" =
  let res =
    Camlzilla_kernel.Memmem.String.memmem
      ~haystack:"abcabcabcabc"
      ~haystack_len:#12L
      ~haystack_pos:#0L
      ~needle:"abc"
      ~needle_len:#3L
      ~needle_pos:#0L
    |> I64.Option.to_option
  in
  print_s [%message (res : int64 option)];
  [%expect {| (res (0)) |}]
;;

let%expect_test "basic example" =
  let haystack = "y\148xp\255Xf\014iKaCaTj2\212" in
  let needle = "XP" in
  let res =
    Camlzilla_kernel.Memmem.String.memmem
      ~haystack
      ~haystack_len:(String.length haystack |> I64.of_int)
      ~haystack_pos:#0L
      ~needle
      ~needle_len:(String.length needle |> I64.of_int)
      ~needle_pos:#0L
    |> I64.Option.to_option
  in
  print_s [%message (res : int64 option)];
  [%expect {| (res ()) |}]
;;

let%expect_test "quickcheck" =
  Quickcheck.test
    [%quickcheck.generator: string * string]
    ~shrinker:[%quickcheck.shrinker: string * string]
    ~sexp_of:[%sexp_of: string * string]
    ~f:(fun (needle, haystack) ->
      let memmem_pos =
        Camlzilla_kernel.Memmem.String.memmem
          ~haystack
          ~haystack_len:(String.length haystack |> I64.of_int)
          ~haystack_pos:#0L
          ~needle
          ~needle_len:(String.length needle |> I64.of_int)
          ~needle_pos:#0L
        |> I64.Option.to_option
      in
      let core_pos =
        String.substr_index haystack ~pattern:needle |> Option.map ~f:Int64.of_int
      in
      [%test_pred: core_pos:int64 option * memmem_pos:int64 option]
        (fun (~core_pos, ~memmem_pos) ->
          [%compare.equal: int64 option] core_pos memmem_pos)
        (~core_pos, ~memmem_pos))
;;
