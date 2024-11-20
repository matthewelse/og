open! Base
open Ppxlib

let for_unboxed =
  Extension.declare
    "i64"
    Extension.Context.Expression
    (let open Ast_pattern in
     single_expr_payload (pexp_for (ppat_var __') __ __ __ __))
    (fun ~loc ~path:_ ident from to_ direction_flag what_to_do ->
      let pident = Ast_builder.Default.ppat_var ~loc:Location.none ident in
      let eident =
        Ast_builder.Default.pexp_ident
          ~loc:Location.none
          (Ast_builder.Default.Located.map_lident ident)
      in
      let precondition, loop_footer =
        match direction_flag with
        | Upto ->
          [%expr I64.O.(![%e eident] <= [%e to_])], [%expr I64.Ref.incr [%e eident]]
        | Downto ->
          [%expr I64.O.(![%e eident] >= [%e to_])], [%expr I64.Ref.decr [%e eident]]
      in
      [%expr
        let [%p pident] = I64.Ref.create_local [%e from] in
        while [%e precondition] do
          let () =
            let [%p pident] = I64.Ref.get [%e eident] in
            [%e what_to_do]
          in
          [%e loop_footer]
        done])
;;

let () = Driver.V2.register_transformation ~extensions:[ for_unboxed ] "u"
