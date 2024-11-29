open! Core
open! Import

type t =
  | Stdin
  | Files_recursively_under of string
[@@deriving sexp_of]

let should_ignore path =
  (* TODO: better support for files to ignore, e.g. globs, .gitignore, etc. *)
  let base = Filename.basename path in
  Char.equal '.' base.[0]
  || String.is_suffix base ~suffix:"~"
  || String.equal base "_build"
;;

let iter t ~buffer_size ~max_buffer_size ~f =
  match t with
  | Stdin ->
    f
      "<stdin>"
      (Buffered_reader.stdin ~initial_size:buffer_size ~max_size:max_buffer_size ())
  | Files_recursively_under dir ->
    let q = Queue.create () in
    Queue.enqueue q dir;
    while not (Queue.is_empty q) do
      let dir = Queue.dequeue_exn q in
      let entries = Sys_unix.readdir dir in
      Array.iter entries ~f:(fun entry ->
        let path = dir ^/ entry in
        match Sys_unix.is_directory path with
        | `Yes -> if should_ignore path then () else Queue.enqueue q path
        | `Unknown -> eprint_s [%message "unknown file type" (path : string)]
        | `No ->
          let fd = Core_unix.openfile path ~mode:[ O_RDONLY ] ~perm:0o644 in
          let reader =
            Buffered_reader.of_fd fd ~initial_size:buffer_size ~max_size:max_buffer_size
          in
          if not (Buffered_reader.is_probably_binary reader) then f path reader;
          Core_unix.close fd)
    done
;;
