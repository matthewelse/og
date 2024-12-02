open! Core

(* bitset represented as a list of ocaml 63 bit ints *)

type t = int list [@@deriving compare, hash]

let empty = []

let[@tail_mod_cons] rec singleton i =
  if i < 0
  then failwith "i must be greater than 0"
  else if i < 63
  then [ 1 lsl i ]
  else 0 :: singleton (i - 63)
;;

let add t i =
  let[@tail_mod_cons] rec add_inner t i =
    match t with
    | [] -> singleton i
    | hd :: tl -> if i < 63 then (hd lor (1 lsl i)) :: tl else hd :: add_inner tl (i - 63)
  in
  if i < 0 then failwith "i must be greater than 0" else add_inner t i
;;

let mem t i =
  let rec mem_inner t i =
    match t with
    | [] -> false
    | hd :: tl -> if i < 63 then hd land (1 lsl i) <> 0 else mem_inner tl (i - 63)
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
  let rec fold_inner t list_offset ~init ~f =
    match t with
    | [] -> init
    | hd :: tl ->
      let rec aux i acc ~f =
        let bit = hd land (1 lsl i) in
        let acc = if bit <> 0 then f acc (list_offset + i) else acc in
        if i = 63 then acc else aux (i + 1) acc ~f
      in
      fold_inner tl (list_offset + 63) ~init:(aux 0 init ~f) ~f
  in
  fold_inner t 0 ~init ~f
;;

let iter (t : t) ~(local_ f) = fold t ~init:() ~f:(fun () i -> f i) [@nontail]

let exists t ~(local_ f) =
  With_return.with_return (fun { return } ->
    iter t ~f:(fun i -> if f i then return true);
    false) [@nontail]
;;

let%expect_test "test basic functionality" =
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
    {| 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 |}];
  iter [ 0; 0x1 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| 63 |}];
  iter [ 0; 0x7 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| 63 64 65 |}];
  iter [ 0x1; 0x7 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| 0 63 64 65 |}];
  iter [ 0x4000_0000_0000_0001; 0x7 ] ~f:(fun i -> Printf.printf "%d " i);
  [%expect {| 0 62 63 64 65 |}]
;;

let of_list is = List.fold is ~init:empty ~f:(fun acc i -> add acc i)
let to_list t = fold t ~init:[] ~f:(fun acc i -> i :: acc) |> List.rev
let sexp_of_t t = [%sexp_of: int list] (to_list t)
