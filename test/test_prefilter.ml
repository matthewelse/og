open! Core
open! Unboxed
open! Import

let%expect_test "example" =
  let re =
    Og.Regex.of_string "[abcdefghijklmnopqrstuvwxyz]+_save"
    |> Or_error.ok_exn
    |> Og.Backends.Nfa_hybrid.compile
  in
  let needle = Og.Backends.Nfa_hybrid.look_for_constant re |> Or_null.value_exn in
  print_s [%message (needle : Bigstring.t)];
  [%expect {| (needle _save) |}];
  let reader =
    Buffered_reader.of_string
      ~initial_size:32
      ~max_size:1024
      {|/* SPDX-License-Identifier: GPL-2.0 */
#ifndef __ASM_ARM_SUSPEND_H
#define __ASM_ARM_SUSPEND_H

#include <linux/types.h>

struct sleep_save_sp {
	u32 *save_ptr_stash;
	u32 save_ptr_stash_phys;
};

extern void cpu_resume(void);
extern void cpu_resume_no_hyp(void);
extern void cpu_resume_arm(void);
extern int cpu_suspend(unsigned long, int (*)(unsigned long));
extern void __cpu_suspend_save(u32 *ptr, u32 ptrsz, u32 sp, u32 *save_ptr);

#endif|}
  in
  let needle =
    Slice.create_local needle ~pos:#0L ~len:(Bigstring.length needle |> I64.of_int)
  in
  while
    Buffered_reader.ensure_line reader;
    let chunk = Buffered_reader.peek reader in
    match%optional_u.I64.Option
      Camlzilla.Memmem.Bigstring.memmem
        ~needle:(Slice.Expert.bytes needle)
        ~needle_pos:#0L
        ~needle_len:(Slice.length needle)
        ~haystack:(Slice.Expert.bytes chunk)
        ~haystack_pos:(Slice.Expert.pos chunk)
        ~haystack_len:(Slice.Expert.len chunk)
    with
    | None ->
      print_s
        [%message
          "not found" (Slice.to_string needle : string) (Slice.to_string chunk : string)];
      (* If the string isn't found, then rewind to the end of the previous line, and
         search from there. *)
      (match%optional_u.I64.Option Slice.rmemchr chunk '\n' with
       | None ->
         (* The regex might extend over this line, so we have to assume taht there's a
            match on this line.. *)
         false
       | Some pos ->
         Buffered_reader.consume reader I64.O.(pos + #1L);
         true)
    | Some pos ->
      print_s
        [%message
          "found"
            (pos : i64)
            (Slice.to_string needle : string)
            (Slice.to_string chunk : string)];
      (* If the string is found, rewind to the end of the previous line before the match,
         and search from there. *)
      (match%optional_u.I64.Option
         Slice.rmemchr (Slice.unsafe_slice chunk ~pos:#0L ~len:pos) '\n'
       with
       | None -> false
       | Some pos ->
         Buffered_reader.consume reader pos;
         false)
  do
    ()
  done;
  Buffered_reader.line reader ~f:(fun line -> print_endline (Slice.to_string line));
  [%expect
    {|
    ("not found"
      ("Slice.to_string needle" _save)
      ("Slice.to_string chunk"
       "/* SPDX-License-Identifier: GPL-2.0 */\n#ifndef __ASM_ARM_SUSPEND"))
    ("not found"
      ("Slice.to_string needle" _save)
      ("Slice.to_string chunk"
       "#ifndef __ASM_ARM_SUSPEND_H\n#define __ASM_ARM_SUSPEND_H\n\n#includ"))
    (found
      (pos                      38)
      ("Slice.to_string needle" _save)
      ("Slice.to_string chunk"
       "#include <linux/types.h>\n\nstruct sleep_save_sp {\n\tu32 *save_ptr_"))
    |}]
;;
