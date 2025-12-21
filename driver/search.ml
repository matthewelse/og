open! Core
open! Unboxed
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
  ~impl:_ (* FIXME: use impl *)
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
  let stats = Stats.zero () in
  let%bind.Or_error re = Og.Regex.of_string pattern in
  let regex = Og.Backends.Nfa_hybrid.compile re in
  Source.iter source ~buffer_size ~max_buffer_size ~f:(fun _path reader ->
    if Buffered_reader.is_probably_binary reader ~num_bytes_to_read:#3L
    then ()
    else (
      Stats.on_file_searched stats;
      while
        try
          (match Og.Backends.Nfa_hybrid.look_for_constant regex with
           | None -> ()
           | Some needle ->
             let needle =
               Slice.create_local
                 needle
                 ~pos:#0L
                 ~len:(Bigstring.length needle |> I64.of_int)
             in
             while
               let chunk = Buffered_reader.peek reader in
               match%optional_u.I64.Option
                 Camlzilla_kernel.Memmem.Bigstring.memmem
                   ~needle:(Slice.Expert.bytes needle)
                   ~needle_pos:#0L
                   ~needle_len:(Slice.length needle)
                   ~haystack:(Slice.Expert.bytes chunk)
                   ~haystack_pos:(Slice.Expert.pos chunk)
                   ~haystack_len:(Slice.Expert.len chunk)
               with
               | None ->
                 (* If the string isn't found, then rewind to the end of the previous line, and
                    search from there. *)
                 (match%optional_u.I64.Option Slice.rmemchr chunk '\n' with
                  | None ->
                    (* The regex might extend over this line, so we have to assume taht there's a
                       match on this line.. *)
                    false
                  | Some pos ->
                    Buffered_reader.consume reader I64.O.(pos + #1L);
                    true)
               | Some pos ->
                 (* If the string is found, rewind to the end of the previous line before the match,
                     and search from there. *)
                 (match%optional_u.I64.Option
                    Slice.rmemchr (Slice.unsafe_slice chunk ~pos:#0L ~len:pos) '\n'
                  with
                  | None -> false
                  | Some pos ->
                    Buffered_reader.consume reader pos;
                    false)
             do
               ()
             done);
          Buffered_reader.line reader ~f:(fun [@inline always] line ->
            Stats.add_bytes_searched stats (Slice.length line);
            let does_match = Og.Backends.Nfa_hybrid.eval regex line in
            if does_match then Stats.on_match stats;
            ());
          true
        with
        | End_of_file -> false
      do
        ()
      done));
  Stats.dump stats;
  Ok ()
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
          (optional_with_default 65536 int)
          ~doc:"BYTES number of bytes to pre-allocate into a buffer."
      and max_buffer_size =
        flag
          "max-buffer-size"
          (optional_with_default (1024 * 1024 * 1024) int)
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
