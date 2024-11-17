open! Core
open! Import

type t = int64# iarray

let array_ix c = Char.to_int c land 3
let bit_ix c = Char.to_int c lsr 2
let bit c = Int64_u.shift_left #1L (bit_ix c)

let%expect_test "bits" =
  List.map Char.all ~f:(fun c ->
    let bit = bit c |> Int64_u.to_int64 in
    let ix = array_ix c in
    bit, ix)
  |> List.exn_if_dup ~compare:[%compare: Int64.t * int] ~to_sexp:[%sexp_of: Int64.t * int];
  [%expect {| |}]
;;

let of_list cs =
  let arr = [| #0L; #0L; #0L; #0L |] in
  List.iter cs ~f:(fun c ->
    let ix = array_ix c in
    assert (ix <= 3 && ix >= 0);
    Array.unsafe_set arr ix (Int64_u.logor (bit c) (Array.unsafe_get arr ix)));
  Iarray.unsafe_of_array arr
;;

let mem t c =
  let bits = Iarray.unsafe_get t (array_ix c) in
  not (Int64_u.equal (Int64_u.logand bits (bit c)) #0L)
;;

let sexp_of_t t = Char.all |> List.filter ~f:(mem t) |> [%sexp_of: char list]