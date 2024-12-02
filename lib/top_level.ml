open! Core
open! Import

module Stats = struct
  type t =
    { mutable bytes_searched : I64.t
    ; mutable num_matches : I64.t
    ; mutable files_searched : I64.t
    }

  let zero () = { bytes_searched = #0L; num_matches = #0L; files_searched = #0L }

  let[@inline always] on_match t =
    let open I64.O in
    t.num_matches <- t.num_matches + #1L
  ;;

  let[@inline always] add_bytes_searched t bytes =
    let open I64.O in
    t.bytes_searched <- t.bytes_searched + bytes
  ;;

  let on_file_searched t =
    let open I64.O in
    t.files_searched <- t.files_searched + #1L
  ;;

  let dump t =
    print_endline [%string "files searched: %{t.files_searched#I64}"];
    print_endline [%string "bytes searched: %{t.bytes_searched#I64}"];
    print_endline [%string "matches: %{t.num_matches#I64}"]
  ;;
end

let main
  ~pattern
  ~count:_
  ~impl:_
  ~buffer_size
  ~max_buffer_size
  ~input_source
  ~show_stats:_
  ~quiet:_
  =
  let source : Source.t =
    match input_source with
    | "-" -> Stdin
    | input_source -> Files_recursively_under input_source
  in
  let search_pattern = Slice.Search_pattern.create (Slice.of_string pattern) in
  let stats = Stats.zero () in
  Source.iter source ~buffer_size ~max_buffer_size ~f:(fun _path reader ->
    Stats.on_file_searched stats;
    while
      try
        Buffered_reader.chunk reader ~f:(fun [@inline always] chunk ->
          Stats.add_bytes_searched stats (Slice.length chunk);
          Slice.Search_pattern.indexes
            search_pattern
            chunk
            ~f:(fun [@inline always] _match_offset_within_slice ->
              Stats.on_match stats;
              (* TODO: If [match_offset_within_file > last_newline_offset], we're on a
                 new line, so increase the number of matched lines.

                 Then, [memchr] until we find the next newline (or the end of
                 the buffer, which is implicitly a newline, and update that.) *)
              ()));
        true
      with
      | End_of_file -> false
    do
      ()
    done);
  Stats.dump stats;
  0
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
        Stdlib.exit
        @@ main
             ~pattern
             ~count
             ~impl
             ~buffer_size
             ~max_buffer_size
             ~input_source
             ~show_stats
             ~quiet]
;;
