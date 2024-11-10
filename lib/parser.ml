open! Core
open! Import

type t =
  { input : string
  ; mutable offset : int
  }

let create input = { input; offset = 0 }

let peek t =
  if t.offset < String.length t.input then exclave_ Some t.input.[t.offset] else None
;;

let take t = exclave_
  let result = peek t in
  if Option.is_some result then t.offset <- t.offset + 1;
  result
;;

let is_empty t = t.offset >= String.length t.input

let drop_exn t =
  assert (not (is_empty t));
  t.offset <- t.offset + 1
;;
