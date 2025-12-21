open! Core
open Unboxed
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

external open_ : path:string -> flags:i32 -> i32 = "open" "open"
external close : i32 -> i32 = "close" "close"

let iter t ~buffer_size ~max_buffer_size ~f =
  match t with
  | Stdin ->
    f
      "<stdin>"
      (Buffered_reader.stdin ~initial_size:buffer_size ~max_size:max_buffer_size ())
  | Files_recursively_under dir_path ->
    let gitignore = Glob_ignore.create () in
    let q = Queue.create () in
    Queue.enqueue q dir_path;
    while not (Queue.is_empty q) do
      let dir_path = Queue.dequeue_exn q in
      (* print_endline [%string "next: %{dir_path}"]; *)
      let dir = Og_utils.Fs.Dir.open_ dir_path in
      match%optional_u.Fs.Dir.Option dir with
      | None -> ()
      | Some dir ->
        Fs.Dir.iter_entries dir ~f:(fun entry ->
          let name = Fs.Dirent.name entry in
          let path = String.concat [ dir_path; "/"; name ] in
          if (String.equal [@mode local]) name ".gitignore"
          then Glob_ignore.parse_gitignore gitignore ~dir:dir_path ~gitignore_path:path;
          match Fs.Dirent.is_regular_file entry with
          | false ->
            (* TODO: handle symlinks *)
            if is_hidden_file path || Glob_ignore.should_ignore gitignore path
            then ()
            else Queue.enqueue q path
          | true ->
            (* print_s [%message "opening" (path : string)]; *)
            if not (Glob_ignore.should_ignore gitignore path)
            then (
              let fd = open_ ~path ~flags:#0l |> Core_unix.File_descr.of_i32 in
              let reader =
                Buffered_reader.of_fd
                  fd
                  ~initial_size:buffer_size
                  ~max_size:max_buffer_size
              in
              if not (Buffered_reader.is_probably_binary reader ~num_bytes_to_read:#64L)
              then f path reader;
              close (Core_unix.File_descr.to_i32 fd) |> (ignore : i32 -> unit))
            else print_s [%message "skipping path" (path : string)]);
        (match Fs.Dir.close dir with
         | (_ : Fs.vnit) -> ())
      (* TODO: enqueue something to indicate that you don't need to pay attention to a particular gitignore anymore *)
    done
;;
