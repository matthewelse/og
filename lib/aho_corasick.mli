open! Core
open! Import

type t

val of_strings : string list -> t
val of_slices : Slice.t list -> t
val is_accepting_state : t @ local -> Trie.Id.t -> bool
val matches_slice : t @ local -> Slice.t @ local -> bool
val matches_string : t @ local -> string @ local -> bool
