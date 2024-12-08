open! Core
open! Import

module Builder = struct
  type t =
    { trie : Trie.t
    ; failure_links : Trie.Id.t Trie.Id.Table.t
    ; output_links : Trie.Id.t list Trie.Id.Table.t
    }
  [@@deriving sexp_of]

  let generate_failure_links (trie : Trie.t) =
    let failure_links = Trie.Id.Table.create () in
    let output_links =
      let tbl = Trie.Id.Table.create () in
      Trie.iter_states trie ~f:(fun id ->
        if Trie.is_accepting_state trie id then Hashtbl.set tbl ~key:id ~data:[ id ]);
      tbl
    in
    let nodes_to_look_at = Queue.create () in
    Queue.enqueue nodes_to_look_at (None, Trie.root_state, Trie.root_state, 0);
    while
      match Queue.dequeue nodes_to_look_at with
      | None -> false
      | Some (last_char, node, parent_failure_node, depth) ->
        let failure_link =
          match last_char with
          | None -> Trie.root_state
          | Some last_char ->
            if depth = 1
            then Trie.root_state
            else Trie.next_state trie parent_failure_node last_char
        in
        Hashtbl.change output_links node ~f:(fun prev_accepting_states ->
          let prev_accepting_states = Option.value prev_accepting_states ~default:[] in
          let failure_link_accepting_states =
            Option.value (Hashtbl.find output_links failure_link) ~default:[]
          in
          let accepting_states = prev_accepting_states @ failure_link_accepting_states in
          if List.is_empty accepting_states then None else Some accepting_states);
        Hashtbl.add_exn failure_links ~key:node ~data:failure_link;
        Trie.iter_children trie node ~f:(fun char child_node ->
          Queue.enqueue nodes_to_look_at (Some char, child_node, failure_link, depth + 1));
        true
    do
      ()
    done;
    failure_links, output_links
  ;;

  let of_slices slices =
    let trie = Trie.of_slices slices in
    let failure_links, output_links = generate_failure_links trie in
    { trie; failure_links; output_links }
  ;;

  let of_strings strings =
    let trie = Trie.of_strings strings in
    let failure_links, output_links = generate_failure_links trie in
    { trie; failure_links; output_links }
  ;;
end

type t =
  { trie : Trie.t
  ; failure_links : Trie.Id.t iarray
  ; output_links : Trie.Id.t iarray iarray
  }

let of_builder (builder : Builder.t) =
  let failure_links =
    Iarray.construct
      ~len:(Trie.num_states builder.trie)
      ~default:Trie.root_state
      ~f:(fun failure_links ->
        for i = 0 to Array.length failure_links - 1 do
          let failure_link =
            Hashtbl.find_exn builder.failure_links (Trie.Id.of_int_exn i)
          in
          failure_links.(i) <- failure_link
        done)
  in
  let output_links =
    Iarray.construct
      ~len:(Trie.num_states builder.trie)
      ~default:[::]
      ~f:(fun output_links ->
        Hashtbl.iteri builder.output_links ~f:(fun ~key:from_state ~data:to_states ->
          output_links.(Trie.Id.to_int_exn from_state) <- Iarray.of_list to_states) [@nontail
                                                                                      ])
  in
  { trie = builder.trie; failure_links; output_links }
;;

let of_strings strings = Builder.of_strings strings |> of_builder
let of_slices strings = Builder.of_slices strings |> of_builder

let is_accepting_state t state =
  Iarray.length (Iarray.get t.output_links (Trie.Id.to_int_exn state)) <> 0
;;

module type Stringy = sig
  type t

  val length : t @ local -> int64#
  val unsafe_get : t @ local -> int64# -> char
end

let[@inline always] matches_stringy
  (type s)
  (module Stringy : Stringy with type t = s)
  t
  (string : s)
  =
  let local_ state : Trie.Id.t ref = ref Trie.root_state in
  let local_ index = I64.Ref.create_local #0L in
  while I64.O.(!index < Stringy.length string) && not (is_accepting_state t !state) do
    let c = Stringy.unsafe_get string I64.O.(!index) in
    let next_state = Trie.next_state t.trie !state c in
    if Trie.Id.equal next_state Trie.root_state
       && not (Trie.Id.equal !state Trie.root_state)
    then state := Iarray.unsafe_get t.failure_links (Trie.Id.to_int_exn !state)
    else (
      state := next_state;
      I64.Ref.incr index)
  done;
  is_accepting_state t !state
;;

let matches_string t string =
  matches_stringy
    (module struct
      include String

      let length t = length t |> I64.of_int
      let unsafe_get t idx = unsafe_get t (I64.to_int_trunc idx)
    end)
    t
    string
;;

let matches_slice t slice = matches_stringy (module Slice) t slice

let%expect_test "check string matching" =
  let strings = [ "meet"; "meat"; "eating"; "tiny"; "in"; "eat" ] in
  let t = of_strings strings in
  List.iter strings ~f:(fun string -> assert (matches_string t string));
  List.iter [ "   meet"; "meat   " ] ~f:(fun string -> assert (matches_string t string));
  List.iter [ "me"; "eek" ] ~f:(fun string -> assert (not (matches_string t string)));
  [%expect {| |}]
;;

let output_graphviz t out_channel =
  Out_channel.output_string out_channel "digraph G {\n";
  let depth_table = Trie.Id.Table.create () in
  let nodes_to_look_at = Queue.create () in
  Queue.enqueue nodes_to_look_at (Trie.root_state, 0);
  while
    match Queue.dequeue nodes_to_look_at with
    | None -> false
    | Some (node, depth) ->
      Hashtbl.add_exn depth_table ~key:node ~data:depth;
      Trie.iter_children t.trie node ~f:(fun char child_node ->
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
  Iarray.iteri t.failure_links ~f:(fun node failure_link ->
    Out_channel.output_string
      out_channel
      (Printf.sprintf
         "  %d -> %d [style=dashed];\n"
         node
         (Trie.Id.to_int_exn failure_link)));
  Iarray.iteri t.output_links ~f:(fun node output_links ->
    Iarray.iter output_links ~f:(fun output_link ->
      Out_channel.output_string
        out_channel
        (Printf.sprintf
           "  %d -> %d [color=blue];\n"
           node
           (Trie.Id.to_int_exn output_link))));
  Out_channel.output_string out_channel "}\n"
;;

let%expect_test "aho corasick -> graphviz" =
  let t = of_strings [ "meet"; "meat"; "eating"; "tiny"; "in"; "eat" ] in
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
      0 -> 0 [style=dashed];
      1 -> 0 [style=dashed];
      2 -> 7 [style=dashed];
      3 -> 0 [style=dashed];
      4 -> 13 [style=dashed];
      5 -> 8 [style=dashed];
      6 -> 9 [style=dashed];
      7 -> 0 [style=dashed];
      8 -> 0 [style=dashed];
      9 -> 13 [style=dashed];
      10 -> 14 [style=dashed];
      11 -> 15 [style=dashed];
      12 -> 0 [style=dashed];
      13 -> 0 [style=dashed];
      14 -> 17 [style=dashed];
      15 -> 18 [style=dashed];
      16 -> 0 [style=dashed];
      17 -> 0 [style=dashed];
      18 -> 0 [style=dashed];
      4 -> 4 [color=blue];
      6 -> 6 [color=blue];
      6 -> 9 [color=blue];
      9 -> 9 [color=blue];
      11 -> 18 [color=blue];
      12 -> 12 [color=blue];
      15 -> 18 [color=blue];
      16 -> 16 [color=blue];
      18 -> 18 [color=blue];
    }
    |}]
;;
