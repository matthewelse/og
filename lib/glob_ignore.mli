type t

val create : unit -> t
val should_ignore : t -> string -> bool
val parse_gitignore : t -> dir:string -> gitignore_path:string -> unit
