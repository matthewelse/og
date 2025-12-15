open! Core
open! Import

let command =
  Command.basic_or_error
    ~summary:"Dump the output of parsing a regex"
    [%map_open.Command
      let regex = anon ("REGEX" %: string)
      and input = flag "input" (optional string) ~doc:"STRING input" in
      fun () ->
        let open Expect_test_helpers_base in
        let%bind.Or_error regex = Og.Regex.of_string regex in
        print_s [%message (regex : Og.Regex.t)];
        let nfa = Og.Backends.Nfa_vm.compile regex in
        print_s [%message (nfa : Og.Backends.Nfa_vm.t)];
        (match input with
         | None -> ()
         | Some input ->
           let did_match = Og.Backends.Nfa_vm.eval nfa (Slice.of_string input) in
           print_endline [%string "did_match = %{did_match#Bool}"]);
        Ok ()]
;;
