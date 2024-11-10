open! Core

type 'a t = 'a iarray

external unsafe_array_of_iarray : 'a iarray -> 'a array = "%identity"
external unsafe_iarray_of_array : 'a array -> 'a iarray = "%identity"

let sexp_of_t sexp_of_a t = [%sexp_of: a Array.t] (unsafe_array_of_iarray t)
let exists t ~f = Array.exists (unsafe_array_of_iarray t) ~f
let of_list t = Array.of_list t |> unsafe_iarray_of_array
let fold t ~init ~f = Array.fold (unsafe_array_of_iarray t) ~init ~f
let unsafe_of_array = unsafe_iarray_of_array
let get_exn t n = Array.get (unsafe_array_of_iarray t) n
let map t ~f = Array.map (unsafe_array_of_iarray t) ~f |> unsafe_iarray_of_array
