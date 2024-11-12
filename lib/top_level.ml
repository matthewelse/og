open! Core

let command =
  Command.basic_or_error
    ~summary:"recursively search the current directory for lines matching a pattern"
    [%map_open.Command
      let pattern = flag "e" (required string) ~doc:"PATTERN"
      and input_source = anon (maybe_with_default "-" ("INPUT" %: string))
      and count =
        flag "c" no_arg ~doc:" count matching input lines without printing matches."
        (* and buffer_size =
        flag
          "buffer-size"
          (optional_with_default 65536 int)
          ~doc:"BYTES number of bytes to pre-allocate into a buffer." *)
      in
      fun () ->
        let open Or_error.Let_syntax in
        let in_channel =
          match input_source with
          | "-" -> In_channel.stdin
          | input_source -> In_channel.create input_source
        in
        let%bind regex = Regex.of_string pattern in
        (* eprint_s [%message (regex : Regex.t)]; *)
        let compiled = Regex.compile regex in
        (* eprint_s [%message (nfa : Nfa.t) (target_state : Nfa.State.t)]; *)
        let num_matches = ref 0 in
        (* let timers = Dynarray.create () in
        Dynarray.ensure_capacity timers 200_000; *)
        (* let buffer = Bytes.create buffer_size in *)
        while
          try
            (* let start = Time_ns.now () in *)
            let line = Stdlib.input_line in_channel in
            (* let middle = Time_ns.now () in *)
            if Regex.Compiled.matches compiled (Slice.create_local line)
            then (
              incr num_matches;
              if not count then print_endline line);
            (* let end_ = Time_ns.now () in *)
            (* Dynarray.add_last timers (start, middle, end_); *)
            true
          with
          | End_of_file -> false
        do
          ()
        done;
        (* let total_time_input, total_time_match =
          Dynarray.fold_left
            (fun (input, match_) (start, middle, end_) ->
              ( Time_ns.Span.( + ) input (Time_ns.diff middle start)
              , Time_ns.Span.( + ) match_ (Time_ns.diff end_ middle) ))
            (Time_ns.Span.zero, Time_ns.Span.zero)
            timers
        in
        eprint_s
          [%message
            (total_time_input : Time_ns.Span.t) (total_time_match : Time_ns.Span.t)]; *)
        if !num_matches > 0
        then (
          if count then print_endline (Int.to_string !num_matches);
          Ok ())
        else Or_error.error_string "no matches"]
;;
