open! Core
open! Import

let command =
  Command.basic_or_error
    ~summary:"Dump the output of parsing a regex"
    [%map_open.Command
      let regex = anon ("REGEX" %: string) in
      fun () ->
        let%bind.Or_error regex = Og.Regex.of_string regex in
        print_s [%message (regex : Og.Regex.t)];
        let nfa = Og.Backends.Nfa_hybrid.compile regex in
        print_s [%message (nfa : Og.Backends.Nfa_hybrid.t)];
        Ok ()]
;;
