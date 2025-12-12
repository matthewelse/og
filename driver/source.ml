open! Core
open! Import

type t =
  | Stdin
  | Files_recursively_under of string
[@@deriving sexp_of]

let is_hidden_file path =
  (* TODO: better support for files to ignore, e.g. globs, .gitignore, etc. *)
  let base = Filename.basename path in
  Char.equal '.' base.[0] || String.is_suffix base ~suffix:"~"
;;

let iter t ~buffer_size ~max_buffer_size ~f =
  match t with
  | Stdin ->
    f
      "<stdin>"
      (Buffered_reader.stdin ~initial_size:buffer_size ~max_size:max_buffer_size ())
  | Files_recursively_under dir ->
    let gitignore = Glob_ignore.create () in
    let q = Queue.create () in
    Queue.enqueue q dir;
    while not (Queue.is_empty q) do
      let dir = Queue.dequeue_exn q in
      let entries = Sys_unix.readdir dir in
      let gitignore_file = Array.find entries ~f:(String.equal ".gitignore") in
      (match gitignore_file with
       | None -> ()
       | Some gitignore_file ->
         Glob_ignore.parse_gitignore gitignore ~dir ~gitignore_path:(dir ^/ gitignore_file));
      Array.iter entries ~f:(fun entry ->
        let path = dir ^/ entry in
        match Sys_unix.is_directory path with
        | `Yes ->
          if is_hidden_file path || Glob_ignore.should_ignore gitignore path
          then print_s [%message "Skipping directory" (path : string)]
          else Queue.enqueue q path
        | `Unknown -> eprint_s [%message "Unknown file type. ignoring" (path : string)]
        | `No ->
          if not (Glob_ignore.should_ignore gitignore path)
          then (
            let fd = Core_unix.openfile path ~mode:[ O_RDONLY ] ~perm:0o644 in
            let reader =
              Buffered_reader.of_fd fd ~initial_size:buffer_size ~max_size:max_buffer_size
            in
            if not (Buffered_reader.is_probably_binary reader) then f path reader;
            Core_unix.close fd)
          else print_s [%message "skipping path" (path : string)])
      (* TODO: enqueue something to indicate that you don't need to pay attention to a particular gitignore anymore *)
    done
;;
