open! Core
open! Unboxed
open! Import

(* FIXME: iarray *)
type t = i64 array

let array_ix c = Char.to_int c land 3
let bit_ix c = Char.to_int c lsr 2
let bit c = I64.O.(#1L lsl bit_ix c)

let of_list cs : t =
  let t = I64.Array.init 4 ~f:(fun _ -> #0L) in
  List.iter cs ~f:(fun c ->
    let ix = array_ix c in
    Array.unsafe_set t ix I64.O.(bit c lor Array.unsafe_get t ix));
  t
;;

let mem (t : t) c =
  let bits = I64.Array.unsafe_get t (array_ix c) in
  not I64.O.(bits land bit c = #0L)
;;

let sexp_of_t t = Char.all |> List.filter ~f:(mem t) |> [%sexp_of: char list]

let%expect_test "bits" =
  List.map Char.all ~f:(fun c ->
    let bit = bit c |> I64.box in
    let ix = array_ix c in
    bit, ix)
  |> List.exn_if_dup ~compare:[%compare: Int64.t * int] ~to_sexp:[%sexp_of: Int64.t * int];
  [%expect {| |}]
;;
