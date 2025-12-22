open! Core
open! Unboxed
open! Import

type t = #(i64 * i64 * i64 * i64)

let array_ix c = Char.to_int c land 3
let bit_ix c = Char.to_int c lsr 2
let bit c = I64.O.(#1L lsl bit_ix c)

module Ref = struct
  [@@@warning "-69"]

  type nonrec t = { mutable contents : t }
end

let of_list cs : t =
  let t : Ref.t = { contents = #(#0L, #0L, #0L, #0L) } in
  List.iter cs ~f:(fun c ->
    let ix = array_ix c in
    let t : i64 array = Obj.magic t in
    Array.unsafe_set t ix I64.O.(bit c lor Array.unsafe_get t ix));
  t.contents
;;

let mem (#(a, b, c, d) : t) char =
  let bits =
    match array_ix char with
    | 0 -> a
    | 1 -> b
    | 2 -> c
    | 3 -> d
    | _ -> assert false
  in
  not I64.O.(bits land bit char = #0L)
;;

let sexp_of_t t = Char.all |> List.filter ~f:(mem t) |> [%sexp_of: char list]

type boxed = Box of t [@@deriving sexp_of]

let%expect_test "bits" =
  List.map Char.all ~f:(fun c ->
    let bit = bit c |> I64.box in
    let ix = array_ix c in
    bit, ix)
  |> List.exn_if_dup ~compare:[%compare: Int64.t * int] ~to_sexp:[%sexp_of: Int64.t * int];
  [%expect {| |}]
;;
