open! Core
open! Import

type t =
  { trie : Trie.t
  ; failure_links : Trie.Id.t Trie.Id.Table.t
  ; output_links : Trie.Id.t list Trie.Id.Table.t
  }
[@@deriving sexp_of]

let generate_failure_links (trie : Trie.t) =
  let failure_links = Trie.Id.Table.create () in
  let output_links =
    Hashtbl.mapi trie.accepting_states ~f:(fun ~key:id ~data:_ -> [ id ])
  in
  let nodes_to_look_at = Queue.create () in
  Queue.enqueue nodes_to_look_at (None, trie.root_state, trie.root_state, 0);
  while
    match Queue.dequeue nodes_to_look_at with
    | None -> false
    | Some (last_char, node, parent_failure_link, depth) ->
      let children =
        Hashtbl.find trie.states node |> Option.value ~default:Char.Map.empty
      in
      let failure_link =
        match last_char with
        | None -> trie.root_state
        | Some last_char ->
          if depth = 1
          then trie.root_state
          else (
            let parent_failure_node =
              Hashtbl.find trie.states parent_failure_link
              |> Option.value ~default:Char.Map.empty
            in
            match Map.find parent_failure_node last_char with
            | None -> trie.root_state
            | Some failure_node -> failure_node)
      in
      Hashtbl.change output_links node ~f:(fun prev_accepting_states ->
        let prev_accepting_states = Option.value prev_accepting_states ~default:[] in
        let failure_link_accepting_states =
          Option.value (Hashtbl.find output_links failure_link) ~default:[]
        in
        let accepting_states = prev_accepting_states @ failure_link_accepting_states in
        if List.is_empty accepting_states then None else Some accepting_states);
      Hashtbl.add_exn failure_links ~key:node ~data:failure_link;
      Map.iteri children ~f:(fun ~key:char ~data:child_node ->
        Queue.enqueue nodes_to_look_at (Some char, child_node, failure_link, depth + 1));
      true
  do
    ()
  done;
  failure_links, output_links
;;

let create strings =
  let trie = Trie.create () in
  List.iter strings ~f:(Trie.add_string trie);
  let failure_links, output_links = generate_failure_links trie in
  { trie; failure_links; output_links }
;;

let output_graphviz t out_channel =
  Out_channel.output_string out_channel "digraph G {\n";
  let depth_table = Trie.Id.Table.create () in
  let nodes_to_look_at = Queue.create () in
  Queue.enqueue nodes_to_look_at (t.trie.root_state, 0);
  while
    match Queue.dequeue nodes_to_look_at with
    | None -> false
    | Some (node, depth) ->
      Hashtbl.add_exn depth_table ~key:node ~data:depth;
      let children =
        Hashtbl.find t.trie.states node |> Option.value ~default:Char.Map.empty
      in
      Map.iteri children ~f:(fun ~key:char ~data:child_node ->
        Out_channel.output_string
          out_channel
          (Printf.sprintf
             "  %d -> %d [label=\"%c\"];\n"
             (Trie.Id.to_int_exn node)
             (Trie.Id.to_int_exn child_node)
             char);
        Queue.enqueue nodes_to_look_at (child_node, depth + 1));
      true
  do
    ()
  done;
  Hashtbl.iteri t.failure_links ~f:(fun ~key:node ~data:failure_link ->
    Out_channel.output_string
      out_channel
      (Printf.sprintf
         "  %d -> %d [style=dashed];\n"
         (Trie.Id.to_int_exn node)
         (Trie.Id.to_int_exn failure_link)));
  Hashtbl.iteri t.output_links ~f:(fun ~key:node ~data:output_links ->
    List.iter output_links ~f:(fun output_link ->
      Out_channel.output_string
        out_channel
        (Printf.sprintf
           "  %d -> %d [color=blue];\n"
           (Trie.Id.to_int_exn node)
           (Trie.Id.to_int_exn output_link))));
  Out_channel.output_string out_channel "}\n"
;;

let%expect_test "aho corasick -> graphviz" =
  let t = create [ "meet"; "meat"; "eating"; "tiny"; "in"; "eat" ] in
  output_graphviz t Out_channel.stdout;
  [%expect
    {|
    digraph G {
      0 -> 7 [label="e"];
      0 -> 17 [label="i"];
      0 -> 1 [label="m"];
      0 -> 13 [label="t"];
      7 -> 8 [label="a"];
      17 -> 18 [label="n"];
      1 -> 2 [label="e"];
      13 -> 14 [label="i"];
      8 -> 9 [label="t"];
      2 -> 5 [label="a"];
      2 -> 3 [label="e"];
      14 -> 15 [label="n"];
      9 -> 10 [label="i"];
      5 -> 6 [label="t"];
      3 -> 4 [label="t"];
      15 -> 16 [label="y"];
      10 -> 11 [label="n"];
      11 -> 12 [label="g"];
      5 -> 8 [style=dashed];
      16 -> 0 [style=dashed];
      7 -> 0 [style=dashed];
      0 -> 0 [style=dashed];
      12 -> 0 [style=dashed];
      8 -> 0 [style=dashed];
      17 -> 0 [style=dashed];
      1 -> 0 [style=dashed];
      4 -> 13 [style=dashed];
      13 -> 0 [style=dashed];
      6 -> 9 [style=dashed];
      15 -> 18 [style=dashed];
      2 -> 7 [style=dashed];
      11 -> 15 [style=dashed];
      10 -> 14 [style=dashed];
      18 -> 0 [style=dashed];
      9 -> 13 [style=dashed];
      14 -> 17 [style=dashed];
      3 -> 0 [style=dashed];
      12 -> 12 [color=blue];
      4 -> 4 [color=blue];
      6 -> 6 [color=blue];
      6 -> 9 [color=blue];
      15 -> 18 [color=blue];
      16 -> 16 [color=blue];
      11 -> 18 [color=blue];
      18 -> 18 [color=blue];
      9 -> 9 [color=blue];
    }
    |}]
;;
