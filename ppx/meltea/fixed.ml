open Ppxlib

let structure file structure =
  let typed = Melhole_typed.File.typed file in
  let top =
    List.concat_map
      (fun (item : Ocaml_common.Typedtree.structure_item) ->
        match item.str_desc with
        | Tstr_value (_, bindings) ->
            List.concat_map
              (fun (binding : Ocaml_common.Typedtree.value_binding) -> Ocaml_common.Typedtree.pat_bound_idents binding.vb_pat)
              bindings
        | _ -> [])
      typed.str_items
  in
  let sites = Hashtbl.create 8 in
  let finder =
    {
      Ocaml_common.Tast_iterator.default_iterator with
      expr =
        (fun iterator (expression : Ocaml_common.Typedtree.expression) ->
          (match expression.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident (callee, _, _); _ },
                (Nolabel, Arg { exp_desc = Texp_ident (source, _, _); _ }) :: _ )
            when List.mem (Ocaml_common.Path.name callee) [ "Meltea_runtime.View.rows"; "Meltea_runtime__View.rows" ]
                 && (match source with
                    | Pident ident -> List.exists (Ocaml_common.Ident.same ident) top
                    | Pdot _ -> true
                    | Papply _ | Pextra_ty _ -> false) ->
              Hashtbl.replace sites expression.exp_loc ()
          | _ -> ());
          Ocaml_common.Tast_iterator.default_iterator.expr iterator expression);
    }
  in
  finder.structure finder typed;
  match Hashtbl.length sites with
  | 0 -> structure
  | _ ->
      (object
         inherit Ast_traverse.map as super

         method! expression expression =
           match (expression.pexp_desc, Hashtbl.mem sites expression.pexp_loc) with
           | ( Pexp_apply
                 ( ({ pexp_desc = Pexp_ident ({ txt = Ldot (view, "rows"); _ } as name); _ } as callee),
                   [ source; listed; template; root; refresh; _key; _scanning; project; pieces; outer; selectors ] ),
               true ) ->
               {
                 expression with
                 pexp_desc =
                   Pexp_apply
                     ( { callee with pexp_desc = Pexp_ident { name with txt = Ldot (view, "fixed") } },
                       List.map
                         (fun (label, argument) -> (label, super#expression argument))
                         [ source; listed; template; root; refresh; project; pieces; outer; selectors ] );
               }
           | _ -> super#expression expression
       end)
        #structure structure
