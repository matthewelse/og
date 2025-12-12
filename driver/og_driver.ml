open! Core

let command =
  Command.group ~summary:"og regex command line tool" [ "search", Search.command ]
;;
