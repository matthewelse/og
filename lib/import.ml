include Og_utils

module List = struct
  include Core.List

  let rec iter_local_input (local_ t) ~(local_ f) =
    match t with
    | [] -> ()
    | x :: xs ->
      f x;
      iter_local_input xs ~f
  ;;
end
