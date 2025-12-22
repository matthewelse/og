open! Core
open Unboxed
open Import

module Opcode = struct
  type ('a, 'b) t =
    | Class : (Charset.boxed, unit) t
    | Eol : (unit, unit) t
    | Jump : (int, unit) t
    | Split : (int, int) t
    | Match : (unit, unit) t
end

module Instruction = struct
  type t : value & value & value = T : #(('a, 'b) Opcode.t * 'a * 'b) -> t [@@unboxed]

  let sexp_of_t (T #(opcode, arg1, arg2)) =
    match opcode with
    | Class ->
      let () = arg2 in
      [%sexp Class (arg1 : Charset.boxed)]
    | Eol ->
      let (), () = arg1, arg2 in
      [%sexp Eol]
    | Jump ->
      let () = arg2 in
      [%sexp Jump (arg1 : int)]
    | Split -> [%sexp Split ((arg1, arg2) : int * int)]
    | Match ->
      let (), () = arg1, arg2 in
      [%sexp Match]
  ;;

  let character_class c = T #(Class, Box (Charset.of_list c), ())
  let eol () = T #(Eol, (), ())
  let jump n = T #(Jump, n, ())
  let split first second = T #(Split, first, second)
  let match_ () = T #(Match, (), ())
end

module Vec0 = Vec

module Vec = struct
  type 'a t = ('a Vec.t[@kind value & value & value])

  let sexp_of_t sexp_of_a t = (Vec.sexp_of_t [@kind value & value & value]) sexp_of_a t
  let create = (Vec.create [@kind value & value & value])
  let push_back = (Vec.push_back [@kind value & value & value])
  let length = (Vec.length [@kind value & value & value])
  let set = (Vec.set [@kind value & value & value])
  let unsafe_get = (Vec.unsafe_get [@kind value & value & value])
end

type t = Instruction.t Vec.t [@@deriving sexp_of]

let rec compile_rule (rule : Regex0.Rule.t) ~instructions =
  let open Instruction in
  match rule with
  | String s ->
    String.iter s ~f:(fun c -> Vec.push_back instructions (character_class [ c ]))
  | Opt rule -> compile_opt rule ~instructions
  | Or (left, right) -> compile_or left right ~instructions
  | Rep1 rule -> compile_rep1 rule ~instructions
  | Seq rules -> List.iter rules ~f:(compile_rule ~instructions)
  | Class c ->
    Vec.push_back
      instructions
      (character_class (Char.all |> List.filter ~f:(Character_class.matches c)))
  | Group grp -> Vec.push_back instructions (character_class grp)
  | Neg_group grp ->
    Vec.push_back
      instructions
      (character_class
         (List.filter Char.all ~f:(fun c -> not (List.mem grp c ~equal:Char.equal))))

and compile_opt inner_rule ~instructions =
  let open Instruction in
  let split_idx = Vec.length instructions in
  Vec.push_back instructions (split 0 0);
  compile_rule inner_rule ~instructions;
  let alt_idx = Vec.length instructions in
  Vec.set instructions split_idx (split (split_idx + 1) alt_idx)

and compile_or left right ~instructions =
  let open Instruction in
  let split_idx = Vec.length instructions in
  let left_idx = split_idx + 1 in
  Vec.push_back instructions (split 0 0);
  compile_rule left ~instructions;
  let jump_idx = Vec.length instructions in
  let right_idx = jump_idx + 1 in
  Vec.push_back instructions (jump 0);
  compile_rule right ~instructions;
  let join_idx = Vec.length instructions in
  Vec.set instructions split_idx (split left_idx right_idx);
  Vec.set instructions jump_idx (jump join_idx)

and compile_rep1 rule ~instructions =
  let open Instruction in
  let rep_idx = Vec.length instructions in
  compile_rule rule ~instructions;
  let split_idx = Vec.length instructions in
  Vec.push_back instructions (split rep_idx (split_idx + 1))
;;

let compile (re : Regex0.t) =
  let open Instruction in
  let instructions : Instruction.t Vec.t = Vec.create () in
  let rule : Regex0.Rule.t =
    if not (Flags.mem re.flags Require_sol)
    then Seq [ Opt (Rep1 (Neg_group [])); re.re ]
    else re.re
  in
  compile_rule rule ~instructions;
  if Flags.mem re.flags Require_eol then Vec.push_back instructions (eol ());
  Vec.push_back instructions (match_ ());
  instructions
;;

let eval (t : Instruction.t Vec.t) (slice : Slice.t @ local) =
  let rec aux (t : t) ~pos ~current_thread ~next_thread =
    let did_match =
      let rec process_current_thread idx =
        match Vec0.maybe_get_or_null current_thread idx with
        | Null -> false
        | This pc ->
          [%debug_assert pc >= 0 && pc < Vec.length t];
          let (T instruction) = Vec.unsafe_get t pc in
          (match
             match instruction with
             | #(Class, Box group, ()) ->
               let c = Slice.get slice pos in
               (match c with
                | Null -> ()
                | This c -> if Charset.mem group c then Vec0.push_back next_thread (pc + 1));
               false
             | #(Eol, (), ()) ->
               if I64.O.(pos = Slice.length slice) then Vec0.push_back next_thread (pc + 1);
               false
             | #(Jump, pc, ()) ->
               Vec0.push_back current_thread pc;
               false
             | #(Split, first, second) ->
               Vec0.push_back current_thread first;
               Vec0.push_back current_thread second;
               false
             | #(Match, (), ()) -> true
           with
           | false -> process_current_thread (idx + 1)
           | true -> true)
      in
      process_current_thread 0
    in
    match did_match with
    | true -> true
    | false ->
      if Vec0.is_empty next_thread
      then false
      else (
        Vec0.clear current_thread;
        aux
          t
          ~pos:I64.O.(pos + #1L)
          ~current_thread:next_thread
          ~next_thread:current_thread)
  in
  let current_thread = Vec0.create ~initial_capacity:(Vec.length t) () in
  let next_thread = Vec0.create ~initial_capacity:(Vec.length t) () in
  Vec0.push_back current_thread 0;
  aux t ~pos:#0L ~current_thread ~next_thread [@nontail]
;;
