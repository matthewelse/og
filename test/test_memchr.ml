open Core
open Unboxed

let strings =
  let%bind.Quickcheck.Generator length = Int.gen_incl 0 1024 in
  String.gen_with_length length Char.quickcheck_generator
;;

module Stats = struct
  type t =
    { mutable count : int
    ; mutable found_cases : int
    ; mutable not_found_cases : int
    ; mutable memchr_count : int
    ; mutable memchr2_count : int
    ; mutable memchr3_count : int
    }
  [@@deriving sexp_of]

  let create () =
    { count = 0
    ; found_cases = 0
    ; not_found_cases = 0
    ; memchr_count = 0
    ; memchr2_count = 0
    ; memchr3_count = 0
    }
  ;;
end

module Case = struct
  type t =
    | One of char
    | Two of char * char
    | Three of char * char * char
  [@@deriving quickcheck, sexp_of]

  let match_ t c' =
    match t with
    | One c0 -> Char.equal c0 c'
    | Two (c0, c1) -> Char.equal c0 c' || Char.equal c1 c'
    | Three (c0, c1, c2) -> Char.equal c0 c' || Char.equal c1 c' || Char.equal c2 c'
  ;;

  let memchr t s =
    let open Camlzilla_kernel in
    let len = String.length s |> I64.of_int in
    match t with
    | One c0 -> Memchr.String.memchr s ~pos:#0L ~len c0
    | Two (c0, c1) -> Memchr.String.memchr2 s ~pos:#0L ~len c0 c1
    | Three (c0, c1, c2) -> Memchr.String.memchr3 s ~pos:#0L ~len c0 c1 c2
  ;;

  let test t ~input_string ~(stats : Stats.t) =
    stats.count <- stats.count + 1;
    let core_pos =
      match String.findi input_string ~f:(fun _ c -> match_ t c) with
      | None -> None
      | Some (x, _) -> Some (Int64.of_int x)
    in
    let memchr_pos = memchr t input_string |> I64.Option.to_option in
    (match t with
     | One _ -> stats.memchr_count <- stats.memchr_count + 1
     | Two _ -> stats.memchr2_count <- stats.memchr2_count + 1
     | Three _ -> stats.memchr3_count <- stats.memchr3_count + 1);
    if Option.is_some memchr_pos
    then stats.found_cases <- stats.found_cases + 1
    else stats.not_found_cases <- stats.not_found_cases + 1;
    [%test_pred: core_pos:int64 option * memchr_pos:int64 option]
      (fun (~core_pos:x, ~memchr_pos:y) -> [%compare.equal: int64 option] x y)
      (~core_pos, ~memchr_pos)
  ;;
end

let%expect_test "memchr should produce the same output as [String.index]" =
  let stats = Stats.create () in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 [%quickcheck.generator: Case.t] strings)
    ~shrinker:[%quickcheck.shrinker: Case.t * String.t]
    ~sexp_of:[%sexp_of: Case.t * string]
    ~examples:
      [ One '\x00', ""
      ; Two ('\x00', '\x01'), ""
      ; Three ('\x00', '\x01', '\x02'), ""
      ; Three ('A', 'A', 'A'), "AAAA"
      ; ( Three ('4', 'T', 'u')
        , "XngbcJgSgZOl6(PqlcYU6Ja1\000jaqeQyz7xoWsjm\255\015W7bY1NRchq9W7N\194oIEivW1o" )
      ; ( Three ('v', 't', 'B')
        , "33BShmzwI:LySMWDHWy,wPTn9tcS9FkslS\011VJ0dRgVI j6\255ksXgM4L0n43i8lLQT" )
      ]
    ~f:(fun (case, input_string) -> Case.test case ~input_string ~stats);
  Expect_test_helpers_core.print_s [%sexp (stats : Stats.t)];
  [%expect
    {|
    ((count           10006)
     (found_cases     8886)
     (not_found_cases 1120)
     (memchr_count    3340)
     (memchr2_count   3330)
     (memchr3_count   3336))
    |}]
;;
