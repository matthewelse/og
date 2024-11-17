open! Core
open! Import

module Source = struct
  type t =
    | Stdin
    | Files_recursively_under of string
  [@@deriving sexp_of]

  let is_binary reader =
    (try Buffered_reader.ensure reader 1024 with
     | End_of_file -> ());
    let buf = Buffered_reader.peek reader in
    With_return.with_return (fun { return } ->
      Slice.Bytes.iter buf ~f:(fun byte -> if Char.to_int byte >= 128 then return true);
      false) [@nontail]
  ;;

  let should_ignore path =
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
      let rec loop dir =
        let entries = Sys_unix.readdir dir in
        Array.iter entries ~f:(fun entry ->
          let path = dir ^/ entry in
          match Sys_unix.is_directory path with
          | `Yes -> if should_ignore path then () else loop path
          | `Unknown -> eprint_s [%message "unknown file type" (path : string)]
          | `No ->
            let fd = Core_unix.openfile path ~mode:[ O_RDONLY ] ~perm:0o644 in
            let reader =
              Buffered_reader.of_fd fd ~initial_size:buffer_size ~max_size:max_buffer_size
            in
            if not (is_binary reader) then f path reader;
            Core_unix.close fd)
      in
      loop dir
  ;;
end

let command =
  Command.basic_or_error
    ~summary:"recursively search the current directory for lines matching a pattern"
    [%map_open.Command
      let pattern = anon ("PATTERN" %: string)
      and count =
        flag "c" no_arg ~doc:" count matching input lines without printing matches."
      and impl =
        flag
          "impl"
          (optional_with_default
             Implementation.Nfa_hybrid
             (Arg_type.enumerated_sexpable
                ~case_sensitive:false
                ~accept_unique_prefixes:true
                (module Implementation)))
          ~doc:"IMPL which implementation to use."
      and buffer_size =
        flag
          "buffer-size"
          (optional_with_default 4096 int)
          ~doc:"BYTES number of bytes to pre-allocate into a buffer."
      and max_buffer_size =
        flag
          "max-buffer-size"
          (optional_with_default 65536 int)
          ~doc:"BYTES maximum number of bytes to pre-allocate into the read buffer."
      and input_source = anon (maybe_with_default "." ("INPUT" %: string)) in
      fun () ->
        let open Or_error.Let_syntax in
        let source : Source.t =
          match input_source with
          | "-" -> Stdin
          | input_source -> Files_recursively_under input_source
        in
        let%bind regex = Regex.of_string pattern in
        let compiled = Regex.compile ~impl regex in
        let num_matches = ref 0 in
        Source.iter source ~buffer_size ~max_buffer_size ~f:(fun path reader ->
          let local_ file_matches = ref 0 in
          let local_ line_number = ref 0 in
          while
            try
              Buffered_reader.line reader ~f:(fun line ->
                incr line_number;
                if Regex.Compiled.matches compiled line
                then (
                  incr file_matches;
                  incr num_matches;
                  if not count
                  then (
                    if !file_matches = 1
                    then (
                      if !num_matches > !file_matches then print_endline "";
                      print_endline path);
                    print_endline [%string "%{!line_number#Int}: %{line#Slice}"]));
                true)
            with
            | End_of_file -> false
          do
            ()
          done);
        if !num_matches > 0
        then (
          if count then print_endline (Int.to_string !num_matches);
          Ok ())
        else Or_error.error_string "no matches"]
;;
