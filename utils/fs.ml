open! Core
open Unboxed

type vnit : void

module Dirent = struct
  type t = iptr

  module Option = struct
    type t = iptr

    let is_none (t @ local) = Iptr.O.(t = #0n)
    let unsafe_value t = t

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = unsafe_value
      end
    end
  end

  external name
    :  t
    -> string @ local
    @@ portable
    = "og_utils_dirent_name_local" "og_utils_dirent_name_local"

  external is_regular_file
    :  t
    -> bool
    @@ portable
    = "og_utils_dirent_is_regular_file" "og_utils_dirent_is_regular_file"
  [@@noalloc]
end

module Dir = struct
  type t = iptr

  module Option = struct
    type t = iptr

    let is_none (t @ local) = Iptr.O.(t = #0n)
    let unsafe_value t = t

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = unsafe_value
      end
    end
  end

  external open_ : string @ local -> Option.t @@ portable = "opendir" "opendir"
  [@@noalloc]

  external close : t -> vnit @@ portable = "closedir" "closedir" [@@noalloc]
  external read : t -> Dirent.Option.t @@ portable = "readdir" "readdir" [@@noalloc]

  let iter_entries t ~f =
    let rec aux t ~f =
      match%optional_u.Dirent.Option read t with
      | None -> ()
      | Some entry ->
        f entry;
        aux t ~f
    in
    aux t ~f
  ;;
end
