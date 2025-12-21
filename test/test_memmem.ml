open! Core
open Unboxed
open! Import

let%expect_test "basic example" =
  let res =
    Camlzilla.Memmem.String.memmem
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
    Camlzilla.Memmem.String.memmem
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

module Make (String : sig
    type t

    val of_string : string -> t

    val memmem
      :  needle:t
      -> needle_pos:i64
      -> needle_len:i64
      -> haystack:t
      -> haystack_pos:i64
      -> haystack_len:i64
      -> I64.Option.t
  end) =
struct
  (* FIXME: This is insufficient. It failed to pick up a trivial bug. *)

  let%expect_test "quickcheck" =
    let num_found = ref 0 in
    let num_not_found = ref 0 in
    Quickcheck.test
      [%quickcheck.generator: string * string]
      ~shrinker:[%quickcheck.shrinker: string * string]
      ~sexp_of:[%sexp_of: string * string]
      ~f:(fun (needle, haystack) ->
        let memmem_pos =
          String.memmem
            ~haystack:(String.of_string haystack)
            ~haystack_len:(Core.String.length haystack |> I64.of_int)
            ~haystack_pos:#0L
            ~needle:(String.of_string needle)
            ~needle_len:(Core.String.length needle |> I64.of_int)
            ~needle_pos:#0L
          |> I64.Option.to_option
        in
        let core_pos =
          Core.String.substr_index haystack ~pattern:needle |> Option.map ~f:Int64.of_int
        in
        if Option.is_some core_pos then incr num_found else incr num_not_found;
        [%test_pred: core_pos:int64 option * memmem_pos:int64 option]
          (fun (~core_pos, ~memmem_pos) ->
            [%compare.equal: int64 option] core_pos memmem_pos)
          (~core_pos, ~memmem_pos));
    print_s [%message (!num_found : int) (!num_not_found : int)];
    [%expect {|
      ((!num_found     909)
       (!num_not_found 9091))
      |}]
  ;;
end

module _ = Make (struct
    include String

    let memmem = Camlzilla.Memmem.String.memmem
  end)

module _ = Make (struct
    include Bytes

    let memmem = Camlzilla.Memmem.Bytes.memmem
  end)

module _ = Make (struct
    include Bigstring

    let of_string s = of_string s
    let memmem = Camlzilla.Memmem.Bigstring.memmem
  end)
