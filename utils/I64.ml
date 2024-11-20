open! Core
include Stdlib_upstream_compatible.Int64_u

module O = struct
  let ( land ) = logand
  let ( lor ) = logor
  let ( lxor ) = logxor
  let ( lsl ) = shift_left
  let ( = ) = equal
  let ( <> ) x y = not (equal x y)
  let lnot = lognot
  let ( + ) = add
  let ( - ) = sub
end

let splat c =
  let c = Char.to_int c |> of_int in
  let open O in
  c
  lor (c lsl 8)
  lor (c lsl 16)
  lor (c lsl 24)
  lor (c lsl 32)
  lor (c lsl 40)
  lor (c lsl 48)
  lor (c lsl 56)
;;

let ctz t = Int64.ctz (to_int64 t)
let clz t = Int64.clz (to_int64 t)
