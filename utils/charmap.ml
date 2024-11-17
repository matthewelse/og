open! Core

type 'a t = 'a array [@@deriving sexp_of]

let length = 256
let char_ix c = Char.to_int c
let create default = Array.create ~len:length default

let[@inline] ( .:() ) t c =
  (* SAFETY: [0 <= char_ix c < 256] *)
  Array.unsafe_get t (char_ix c)
;;

let[@inline] ( .:()<- ) t c val_ =
  (* SAFETY: [0 <= char_ix c < 256] *)
  Array.unsafe_set t (char_ix c) val_
;;
