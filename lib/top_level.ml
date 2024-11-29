open! Core
open! Import

let main
  ~pattern
  ~count
  ~impl
  ~buffer_size
  ~max_buffer_size
  ~input_source
  ~show_stats
  ~quiet
  =
  let open Or_error.Let_syntax in
  let source : Source.t =
    match input_source with
    | "-" -> Stdin
    | input_source -> Files_recursively_under input_source
  in
  let%bind regex = Regex.of_string pattern in
  let compiled = Regex.compile ?impl regex in
  let num_matches = ref 0 in
  let bytes_read = I64.Ref.create #0L in
  Source.iter source ~buffer_size ~max_buffer_size ~f:(fun path reader ->
    let local_ file_matches = ref 0 in
    let local_ line_number = ref 0 in
    while
      try
        Buffered_reader.line reader ~f:(fun line ->
          incr line_number;
          I64.Ref.add bytes_read (Slice.length line);
          if Regex.Compiled.matches compiled line
          then (
            incr file_matches;
            incr num_matches;
            if (not count) && not quiet
            then (
              if !file_matches = 1
              then (
                if !num_matches > !file_matches then print_endline "";
                print_endline path);
              print_endline [%string "%{!line_number#Int}: %{line#Slice}"])
            else if not quiet
            then (
              print_string path;
              Stdlib.print_int !line_number;
              print_string ": ";
              Slice.output line stdout;
              Out_channel.newline stdout));
          true)
      with
      | End_of_file -> false
    do
      ()
    done);
  if show_stats
  then (
    let bytes_read = I64.Ref.get bytes_read in
    print_endline [%string "%{!num_matches#Int} matches"];
    print_endline [%string "%{bytes_read#I64} bytes read"]);
  if !num_matches > 0
  then (
    if count then print_endline (Int.to_string !num_matches);
    Ok ())
  else Or_error.error_string "no matches"
;;

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
          (optional
             (Arg_type.enumerated_sexpable
                ~case_sensitive:false
                ~accept_unique_prefixes:true
                (module Implementation)))
          ~doc:"IMPL specific matching implementation to use."
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
      and input_source = anon (maybe_with_default "." ("INPUT" %: string))
      and show_stats = flag "show-stats" no_arg ~doc:" show stats"
      and quiet = flag "quiet" no_arg ~doc:" don't write anything to stdout" in
      fun () ->
        main
          ~pattern
          ~count
          ~impl
          ~buffer_size
          ~max_buffer_size
          ~input_source
          ~show_stats
          ~quiet]
;;
