open! Core

(* bitset represented as a list of ocaml 63 bit ints *)

type t = int list [@@deriving compare, sexp_of]

let empty = []

let rec singleton i =
  if i < 0
  then failwith "i must be greater than 0"
  else if i < 63
  then [ 1 lsl i ]
  else 0 :: singleton (i / 63)
;;

let add t i =
  let rec add_inner t i =
    match t with
    | [] -> singleton i
    | hd :: tl -> if i < 63 then (hd lor (1 lsl i)) :: tl else hd :: add_inner tl (i / 63)
  in
  if i < 0 then failwith "i must be greater than 0" else add_inner t i
;;

let mem t i =
  let rec mem_inner t i =
    match t with
    | [] -> false
    | hd :: tl -> if i < 63 then hd land (1 lsl i) <> 0 else mem_inner tl (i / 63)
  in
  if i < 0 then failwith "i must be greater than 0" else mem_inner t i
;;

let union t1 t2 =
  let rec union_inner t1 t2 =
    match t1, t2 with
    | [], [] -> []
    | [], _ -> t2
    | _, [] -> t1
    | hd1 :: tl1, hd2 :: tl2 -> (hd1 lor hd2) :: union_inner tl1 tl2
  in
  union_inner t1 t2
;;

let fold t ~init ~(local_ f) =
  let rec fold_inner t i ~init ~f =
    match t with
    | [] -> init
    | hd :: tl ->
      let rec fold_bit i bit ~init ~f =
        let trailing_zeros = Ocaml_intrinsics_kernel.Int.count_trailing_zeros bit in
        let bit = bit lsr trailing_zeros in
        if bit = 0
        then fold_inner tl (i + 63) ~init ~f
        else (
          let init = f init (i + trailing_zeros) in
          fold_bit (i + trailing_zeros + 1) (bit lsr 1) ~init ~f)
      in
      fold_bit i hd ~init ~f
  in
  fold_inner t 0 ~init ~f
;;

let iter (t : t) ~(local_ f) = fold t ~init:() ~f:(fun () i -> f i) [@nontail]

let exists t ~(local_ f) =
  With_return.with_return (fun { return } ->
    iter t ~f:(fun i -> if f i then return true);
    false) [@nontail]
;;

let%expect_test "asdf" =
  iter [ 0x0 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| |}];
  iter [ 0x1 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| 0 |}];
  iter [ 0x1111_1111 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| 0 4 8 12 16 20 24 28 |}];
  iter [ 0x0000_ffff_ffff_0000 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect
    {| 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 |}];
  iter [ 0; 0x0000_ffff_ffff_0000 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect
    {| 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 |}]
;;

let of_list is = List.fold is ~init:empty ~f:(fun acc i -> add acc i)
