open! Core
open! Import

type t = int64# iarray

let array_ix c = Char.to_int c land 3
let bit_ix c = Char.to_int c lsr 2
let bit c = I64.O.(#1L lsl bit_ix c)

let of_list cs : t =
  Iarray.construct__bits64 ~len:4 ~default:#0L ~f:(fun arr ->
    List.iter cs ~f:(fun c ->
      let ix = array_ix c in
      Array.unsafe_set arr ix I64.O.(bit c lor Array.unsafe_get arr ix)) [@nontail])
;;

let mem t c =
  let bits = Iarray.unsafe_get t (array_ix c) in
  not I64.O.(bits land bit c = #0L)
;;

let sexp_of_t t = Char.all |> List.filter ~f:(mem t) |> [%sexp_of: char list]

let%expect_test "bits" =
  List.map Char.all ~f:(fun c ->
    let bit = bit c |> I64.to_int64 in
    let ix = array_ix c in
    bit, ix)
  |> List.exn_if_dup ~compare:[%compare: Int64.t * int] ~to_sexp:[%sexp_of: Int64.t * int];
  [%expect {| |}]
;;
