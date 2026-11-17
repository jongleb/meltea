open Ppxlib

let structure file structure =
  let replaced = Hashtbl.create 8 in
  let finder =
    {
      Ocaml_common.Tast_iterator.default_iterator with
      expr =
        (fun iterator (expression : Ocaml_common.Typedtree.expression) ->
          (match expression.exp_desc with
          | Texp_ident (path, _, _) when List.mem (Ocaml_common.Path.name path) [ "Stdlib.float_of_string_opt" ] ->
              Hashtbl.replace replaced expression.exp_loc "float_of_string_opt"
          | _ -> ());
          Ocaml_common.Tast_iterator.default_iterator.expr iterator expression);
    }
  in
  finder.structure finder (Melhole_typed.File.typed file);
  match Hashtbl.length replaced with
  | 0 -> structure
  | _ ->
      (object
         inherit Ast_traverse.map as super

         method! expression expression =
           match (expression.pexp_desc, Hashtbl.find_opt replaced expression.pexp_loc) with
           | Pexp_ident _, Some name ->
               { expression with pexp_desc = Pexp_ident { txt = Ldot (Ldot (Lident "Meltea_runtime", "Numbers"), name); loc = expression.pexp_loc } }
           | _ -> super#expression expression
       end)
        #structure structure
