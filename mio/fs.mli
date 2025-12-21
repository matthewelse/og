@@ portable

open! Core
open! Unboxed

module Dirent : sig
  type t : word

  module Option : sig
    type value := t
    type t : word

    module Optional_syntax : Optional_syntax.S with type t := t and type value := value
  end

  val name : t -> string @ local
  val is_regular_file : t -> bool
end

module Dir : sig
  type t : word

  module Option : sig
    type value := t
    type t : word

    module Optional_syntax : Optional_syntax.S with type t := t and type value := value
  end

  val open_ : string @ local -> Option.t
  val close : t -> unit
  val read : t -> Dirent.Option.t
  val iter_entries : t -> f:(Dirent.t -> unit) @ local -> unit
end
