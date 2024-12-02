{
(* Adapted from [Dune_glob]. *)

open! Core 

type t = Regex0.Rule.t
[@@deriving sexp_of]

let no_slash        : Regex0.Rule.t = Neg_group [ '/' ]
let no_slash_no_dot : Regex0.Rule.t = Neg_group [ '.'; '/' ] 

type stack =
  | Bottom
  | Lbrace of stack
  | Char   of char * stack
  | Re     of Regex0.Rule.t * stack
  | Comma  of stack

let make_group st =
  let rec loop (current_re : Regex0.Rule.t list) (full_res : Regex0.Rule.t list) st =
    match st with
    | Bottom       -> failwith "'}' without opening '{'"
    | Re (re, st)  -> loop (re :: current_re) full_res st
    | Char (c, st) -> loop (Group [ c ] :: current_re) full_res st
    | Comma   st   -> loop [] (Seq current_re :: full_res) st
    | Lbrace  st   -> Re (Regex0.Rule.or_list (Seq current_re :: full_res), st)
  in
  loop [] [] st

let finalize st =
  let rec loop acc st : Regex0.Rule.t =
    match st with
    | Bottom      -> Seq acc
    | Re (re, st) -> loop (re       :: acc) st
    | Char (c, st)  -> loop (Group [ c ] :: acc) st
    | Comma   st  -> loop (Group [ ',' ] :: acc) st
    | Lbrace  _   -> failwith "unclosed '{'"
  in
  let rec try_str (acc : char list) st =
    match st with
    | Bottom -> (Regex0.Rule.String (String.of_list acc))
    | Comma st -> try_str (',' :: acc) st
    | Char (c, st) -> try_str (c :: acc) st
    | st ->
      let re =
        let re = [] in
        match acc with
        | [] -> re
        | _ :: _ -> Regex0.Rule.String (String.of_list acc) :: re
      in
      loop re st
  in
  try_str [] st
}

rule initial = parse
  | "**" { glob (Re (Regex0.Rule.rep (Neg_group []), Bottom)) lexbuf }
  | "*"  { glob (Re (Seq [no_slash_no_dot; Regex0.Rule.rep no_slash], Bottom)) lexbuf }
  | ""   { glob Bottom lexbuf }

and glob st = parse
  | eof
  | '\\' eof      { finalize st }
  | '\\' (_ as c) { glob (Char (c                                 , st)) lexbuf }
  | "**"          { glob (Re (Seq [no_slash_no_dot; Regex0.Rule.rep no_slash] , st)) lexbuf }
  | '*'           { glob (Re (Regex0.Rule.rep no_slash                        , st)) lexbuf }
  | '?'           { glob (Re (no_slash                            , st)) lexbuf }
  | '{'           { glob (Lbrace                                    st ) lexbuf }
  | ','           { glob (Comma                                     st ) lexbuf }
  | '}'           { glob (make_group st)                                 lexbuf }
  | '['           { char_set st                                          lexbuf }
  | ']'           { failwith "']' without opening '['"                          }
  | _ as c        { glob (Char (c                                 , st)) lexbuf }

and char_set st = parse
  | '!' ([^ ']']* as s) "]" { glob (Re (Neg_group (String.to_list s) , st)) lexbuf }
  |     ([^ ']']* as s) "]" { glob (Re (Group (String.to_list s)            , st)) lexbuf }
  | ""                     { failwith "unclosed character set"        }

{
  let parse_string s =
    let lb = Lexing.from_string s in
    match initial lb with
    | re -> Result.Ok re
    | exception Failure msg ->
      Error (Lexing.lexeme_start lb, msg)
}