open Core
open Unboxed

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

module Make (String : sig
    type t [@@deriving quickcheck, sexp_of]

    (* from core *)
    val length : t -> int
    val findi : t -> f:(int -> char -> bool) @ local -> (int * char) option
    val rfindi : ?pos:int -> t -> f:(int -> char -> bool) @ local -> int option
    val of_string : string -> t

    (* from camlzilla *)
    val memchr : t -> pos:i64 -> len:i64 -> char -> I64.Option.t
    val memchr2 : t -> pos:i64 -> len:i64 -> char -> char -> I64.Option.t
    val memchr3 : t -> pos:i64 -> len:i64 -> char -> char -> char -> I64.Option.t
    val rmemchr : t -> pos:i64 -> len:i64 -> char -> I64.Option.t
  end) =
struct
  let s = String.of_string

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
      let len = String.length s |> I64.of_int in
      match t with
      | One c0 -> String.memchr s ~pos:#0L ~len c0
      | Two (c0, c1) -> String.memchr2 s ~pos:#0L ~len c0 c1
      | Three (c0, c1, c2) -> String.memchr3 s ~pos:#0L ~len c0 c1 c2
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
      [%quickcheck.generator: Case.t * String.t]
      ~shrinker:[%quickcheck.shrinker: Case.t * String.t]
      ~sexp_of:[%sexp_of: Case.t * String.t]
      ~examples:
        [ One '\x00', s ""
        ; Two ('\x00', '\x01'), s ""
        ; Three ('\x00', '\x01', '\x02'), s ""
        ; Three ('A', 'A', 'A'), s "AAAA"
        ; ( Three ('4', 'T', 'u')
          , s
              "XngbcJgSgZOl6(PqlcYU6Ja1\000jaqeQyz7xoWsjm\255\015W7bY1NRchq9W7N\194oIEivW1o"
          )
        ; ( Three ('v', 't', 'B')
          , s "33BShmzwI:LySMWDHWy,wPTn9tcS9FkslS\011VJ0dRgVI j6\255ksXgM4L0n43i8lLQT" )
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

  let%expect_test "rmemchr should produce the same output as [String.rfndex]" =
    let input_string = s "6G" in
    let char = '6' in
    let core_pos =
      match String.rfindi input_string ~f:(fun _ c -> Char.equal c char) with
      | None -> None
      | Some x -> Some (Int64.of_int x)
    in
    let memchr_pos =
      String.rmemchr
        input_string
        char
        ~pos:#0L
        ~len:(String.length input_string |> I64.of_int)
      |> I64.Option.to_option
    in
    [%test_pred: core_pos:int64 option * memchr_pos:int64 option]
      (fun (~core_pos:x, ~memchr_pos:y) -> [%compare.equal: int64 option] x y)
      (~core_pos, ~memchr_pos);
    [%expect {| |}]
  
  ;;

  let%expect_test "rmemchr should produce the same output as [String.rfndex]" =
    let stats = Stats.create () in
    Quickcheck.test
      [%quickcheck.generator: char * String.t]
      ~shrinker:[%quickcheck.shrinker: char * String.t]
      ~sexp_of:[%sexp_of: char * String.t]
      ~f:(fun (char, input_string) ->
        stats.count <- stats.count + 1;
        let core_pos =
          match String.rfindi input_string ~f:(fun _ c -> Char.equal c char) with
          | None -> None
          | Some x -> Some (Int64.of_int x)
        in
        let memchr_pos =
          String.rmemchr
            input_string
            char
            ~pos:#0L
            ~len:(String.length input_string |> I64.of_int)
          |> I64.Option.to_option
        in
        stats.memchr_count <- stats.memchr_count + 1;
        if Option.is_some core_pos
        then stats.found_cases <- stats.found_cases + 1
        else stats.not_found_cases <- stats.not_found_cases + 1;
        [%test_pred: core_pos:int64 option * memchr_pos:int64 option]
          (fun (~core_pos:x, ~memchr_pos:y) -> [%compare.equal: int64 option] x y)
          (~core_pos, ~memchr_pos));
    Expect_test_helpers_core.print_s [%sexp (stats : Stats.t)];
    [%expect
      {|
    ((count           10000)
     (found_cases     8257)
     (not_found_cases 1743)
     (memchr_count    10000)
     (memchr2_count   0)
     (memchr3_count   0))
    |}]
  ;;
end

let string_generator =
  let%bind.Quickcheck.Generator length = Int.gen_incl 0 1024 in
  String.gen_with_length length Char.quickcheck_generator
;;

module _ = Make (struct
    include String

    let memchr = Camlzilla_kernel.Memchr.String.memchr
    let memchr2 = Camlzilla_kernel.Memchr.String.memchr2
    let memchr3 = Camlzilla_kernel.Memchr.String.memchr3
    let rmemchr = Camlzilla_kernel.Memchr.String.rmemchr
    let quickcheck_generator = string_generator
    let rfindi ?pos t ~f = rfindi ?pos t ~f
  end)

module _ = Make (struct
    include Bytes

    let memchr = Camlzilla_kernel.Memchr.Bytes.memchr
    let memchr2 = Camlzilla_kernel.Memchr.Bytes.memchr2
    let memchr3 = Camlzilla_kernel.Memchr.Bytes.memchr3
    let rmemchr = Camlzilla_kernel.Memchr.Bytes.rmemchr

    let rfindi ?pos t ~f =
      String.rfindi
        ?pos
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t)
        ~f [@nontail]
    ;;

    let findi t ~f =
      String.findi
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t)
        ~f [@nontail]
    ;;

    let quickcheck_generator = Quickcheck.Generator.map ~f:of_string string_generator
  end)

module _ = Make (struct
    include Bigstring

    let memchr = Camlzilla_kernel.Memchr.Bigstring.memchr
    let memchr2 = Camlzilla_kernel.Memchr.Bigstring.memchr2
    let memchr3 = Camlzilla_kernel.Memchr.Bigstring.memchr3
    let rmemchr = Camlzilla_kernel.Memchr.Bigstring.rmemchr
    let rfindi ?pos t ~f = String.rfindi ?pos (Bigstring.to_string t) ~f
    let findi t ~f = String.findi (to_string t) ~f
    let quickcheck_generator = Quickcheck.Generator.map ~f:of_string string_generator
    let of_string s = of_string s
  end)
