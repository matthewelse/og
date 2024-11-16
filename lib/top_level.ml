open! Core

let command =
  Command.basic_or_error
    ~summary:"recursively search the current directory for lines matching a pattern"
    [%map_open.Command
      let pattern = flag "e" (required string) ~doc:"PATTERN"
      and input_source = anon (maybe_with_default "-" ("INPUT" %: string))
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
      in
      fun () ->
        let open Or_error.Let_syntax in
        let reader =
          match input_source with
          | "-" ->
            Buffered_reader.stdin ~initial_size:buffer_size ~max_size:max_buffer_size ()
          | input_source ->
            Core_unix.openfile input_source ~mode:[ O_RDONLY ] ~perm:0o644
            |> Buffered_reader.of_fd ~initial_size:buffer_size ~max_size:max_buffer_size
        in
        let%bind regex = Regex.of_string pattern in
        let compiled = Regex.compile ~impl regex in
        let num_matches = ref 0 in
        while
          try
            Buffered_reader.line reader ~f:(fun line ->
              if Regex.Compiled.matches compiled line
              then (
                incr num_matches;
                if not count then Slice.print_endline line);
              true)
          with
          | End_of_file -> false
        do
          ()
        done;
        if !num_matches > 0
        then (
          if count then print_endline (Int.to_string !num_matches);
          Ok ())
        else Or_error.error_string "no matches"]
;;
