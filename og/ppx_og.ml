open Base
open! Ppxlib

let profile = ref ""

let is_debug () =
  if String.equal !profile ""
  then failwith "Mode not set"
  else String.equal !profile "dev"
;;

let debug_assert =
  Extension.declare
    "debug_assert"
    Extension.Context.Expression
    (let open Ast_pattern in
     single_expr_payload __)
    (fun ~loc ~path:_ bool ->
      [%expr
        (* Type-check the code, but try not to execute it! *)
        let _ : unit -> bool = fun () -> [%e bool] in
        [%e if is_debug () then [%expr assert [%e bool]] else [%expr ()]]])
;;

let debug_run =
  Extension.declare
    "debug_run"
    Extension.Context.Expression
    (let open Ast_pattern in
     single_expr_payload __)
    (fun ~loc ~path:_ expr ->
      [%expr
        (* Type-check the code, but try not to execute it! *)
        let _ : unit -> unit = fun () -> [%e expr] in
        [%e if is_debug () then [%expr [%e expr]] else [%expr ()]]])
;;

let () =
  Driver.add_arg "-mode" (Stdlib.Arg.Set_string profile) ~doc:"PROFILE dune profile"
;;

let () = Driver.register_transformation ~extensions:[ debug_assert; debug_run ] "ppx_og"
