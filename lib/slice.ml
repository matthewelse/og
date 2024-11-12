open! Core

type t =
  { global_ bytes : string
  ; pos : int
  ; len : int
  }
[@@deriving globalize, sexp_of]

let to_string t = String.sub t.bytes ~pos:t.pos ~len:t.len

let check_bounds t =
  if t.pos + t.len > String.length t.bytes
  then (
    let t = globalize t in
    raise_s [%message "Start index+length out of bounds" (t : t)])
  else if t.pos < 0
  then (
    let t = globalize t in
    raise_s [%message "Start position < 0" (t : t)])
  else if t.len < 0
  then (
    let t = globalize t in
    raise_s [%message "Length < 0" (t : t)])
;;

let create ?(pos = 0) ?len bytes =
  let len = Option.value len ~default:(String.length bytes - pos) in
  let t = { bytes; pos; len } in
  check_bounds t;
  t
;;

let create_local ?(pos = 0) ?len bytes = exclave_
  let len = Option.value len ~default:(String.length bytes - pos) in
  let local_ t = { bytes; pos; len } in
  check_bounds t;
  t
;;

let length t = t.len

let at (local_ t) ix =
  if ix >= t.len || ix < 0 then failwith "index out of bounds" else t.bytes.[t.pos + ix]
;;

let unsafe_at (local_ t) ix = String.unsafe_get t.bytes (t.pos + ix)

let iteri t ~f =
  for i = 0 to t.len - 1 do
    f i (String.unsafe_get t.bytes (i + t.pos))
  done
;;

let unsafe_slice t ~pos ~len = exclave_ { t with pos = t.pos + pos; len }

let slice ?(pos = 0) ?len t = exclave_
  let len = Option.value len ~default:(t.len - pos) in
  let t = unsafe_slice ~pos ~len t in
  check_bounds t;
  t
;;

let memcmp (local_ t) (local_ t') =
  t.len = t'.len
  && (With_return.with_return (fun { return } ->
        for i = 0 to t.len - 1 do
          if not (Char.equal (unsafe_at t i) (unsafe_at t' i)) then return false
        done;
        true) [@nontail])
;;

module Search_pattern = struct
  type slice = t [@@deriving sexp_of]

  type t =
    { pattern : slice
    ; offsets : (int Iarray.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let pattern t = t.pattern

  let create pattern =
    let offsets = Array.init 256 ~f:(fun _ -> length pattern) in
    iteri pattern ~f:(fun i c ->
      if i <> length pattern - 1 then offsets.(Char.to_int c) <- length pattern - i - 1);
    let offsets = Iarray.unsafe_of_array offsets in
    { pattern; offsets }
  ;;

  let index t haystack =
    (* Boyer-Moore-Horspool string matching *)
    if length t.pattern > length haystack
    then None
    else (
      let k = ref 0 in
      let result =
        With_return.with_return (fun { return } ->
          while length haystack - !k >= length t.pattern do
            if memcmp (unsafe_slice ~pos:!k ~len:t.pattern.len haystack) t.pattern
            then return !k
            else
              k
              := !k
                 + Iarray.unsafe_get
                     t.offsets
                     (unsafe_at haystack (!k + length t.pattern - 1) |> Char.to_int)
          done;
          -1)
      in
      if result = -1 then None else exclave_ Some result)
  ;;
end
