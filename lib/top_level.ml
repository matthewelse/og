open! Core

let command =
  Command.basic_or_error
    ~summary:"recursively search the current directory for lines matching a pattern"
    [%map_open.Command
      let pattern = flag "e" (required string) ~doc:"PATTERN"
      and input_source = anon (maybe_with_default "-" ("INPUT" %: string))
      and count =
        flag "c" no_arg ~doc:" count matching input lines without printing matches."
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
        while
          try
            let line = Stdlib.input_line in_channel in
            if Regex.Compiled.matches compiled line
            then (
              incr num_matches;
              if not count then print_endline line);
            true
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
