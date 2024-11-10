open! Core

let command =
  Command.basic_or_error
    ~summary:"recursively search the current directory for lines matching a pattern"
    [%map_open.Command
      let pattern = flag "E" (required string) ~doc:"PATTERN"
      and input_source = anon (maybe_with_default "-" ("INPUT" %: string)) in
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
        let did_match = ref false in
        In_channel.iter_lines in_channel ~f:(fun line ->
          if Regex.Compiled.matches compiled line
          then (
            did_match := true;
            print_endline line));
        if !did_match then Ok () else Or_error.error_string "no matches"]
;;
