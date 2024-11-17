include Stdlib.Dynarray

let sexp_of_t sexp_of_a t = [%sexp_of: a Core.List.t] (to_list t)

let resize t n ~default =
  if length t > n
  then ()
  else
    for _ = length t to n - 1 do
      add_last t (default ())
    done
;;

let to_iarray t =
  if length t = 0 then [::] else Iarray.init (length t) ~f:(fun i -> get t i)
;;
