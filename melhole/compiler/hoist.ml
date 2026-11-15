open Ppxlib
open Ast_builder.Default

type unused = Dropped | Kept

let counter = ref 0
let lifted = ref []

let hoist ~loc ~unused prefix expression =
  incr counter;
  let name = Printf.sprintf "__melhole_%s_%d" prefix !counter in
  lifted := ([%stri let [%p pvar ~loc name] = [%e expression]], unused) :: !lifted;
  evar ~loc name

let declare ~loc prefix declaration =
  incr counter;
  let name = Printf.sprintf "__melhole_%s_%d" prefix !counter in
  let declared = declaration name in
  lifted :=
    ( pstr_type ~loc Recursive
        [
          {
            declared with
            ptype_attributes =
              attribute ~loc ~name:{ txt = "ocaml.warning"; loc } ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-34-69") [] ])
              :: declared.ptype_attributes;
          };
        ],
      Kept )
    :: !lifted;
  name

let structure map items =
  let module Names = Set.Make (String) in
  let ignoring name expression =
    match expression.pexp_desc with
    | Pexp_let
        ( Nonrecursive,
          [ { pvb_pat = { ppat_desc = Ppat_any; _ }; pvb_expr = { pexp_desc = Pexp_ident { txt = Lident used; _ }; _ }; _ } ],
          rest )
      when String.equal used name ->
        Some rest
    | _ -> None
  in
  let pruned =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        let expression = super#expression expression in
        match expression.pexp_desc with
        | Pexp_let
            ( Nonrecursive,
              [
                {
                  pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ };
                  pvb_expr = { pexp_desc = Pexp_function _ | Pexp_constraint ({ pexp_desc = Pexp_function _; _ }, _); _ };
                  _;
                };
              ],
              body ) -> (
            let uses =
              object
                inherit [int * int] Ast_traverse.fold as super

                method! expression expression (all, ignored) =
                  match (expression.pexp_desc, ignoring name expression) with
                  | Pexp_ident { txt = Lident used; _ }, _ when String.equal used name -> (all + 1, ignored)
                  | Pexp_letop { let_; ands; _ }, _
                    when List.exists (fun (operator : binding_op) -> String.equal operator.pbop_op.txt name) (let_ :: ands) ->
                      super#expression expression (all + 1, ignored)
                  | _, Some _ -> super#expression expression (all, ignored + 1)
                  | _, None -> super#expression expression (all, ignored)
              end
            in
            match uses#expression body (0, 0) with
            | all, ignored when all = ignored && ignored > 0 ->
                let unignored =
                  object (self)
                    inherit Ast_traverse.map as super

                    method! expression expression =
                      match ignoring name expression with
                      | Some rest -> self#expression rest
                      | None -> super#expression expression
                  end
                in
                unignored#expression body
            | _, _ -> expression)
        | _ -> expression
    end
  in
  let referenced =
    object
      inherit [Names.t] Ast_traverse.fold as super

      method! longident longident found = super#longident longident (Names.add (Longident.last_exn longident) found)
    end
  in
  let opened =
    object
      inherit [Names.t] Ast_traverse.fold as super

      method! expression expression found =
        match expression.pexp_desc with
        | Pexp_open (_, inner) -> referenced#expression inner (super#expression expression found)
        | _ -> super#expression expression found
    end
  in
  let apart item before =
    let own =
      List.fold_left
        (fun own (hoisted, _) ->
          match hoisted.pstr_desc with
          | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; _ } ]) -> Names.add name own
          | _ -> own)
        Names.empty before
    in
    let hidden =
      Names.inter own (List.fold_left (fun found (hoisted, _) -> opened#structure_item hoisted found) (opened#structure_item item Names.empty) before)
    in
    match Names.is_empty hidden with
    | true -> (before, item)
    | false ->
        let unit =
          String.capitalize_ascii
            (String.map
               (fun character -> match character with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> character | _ -> '_')
               (Filename.remove_extension (Filename.basename item.pstr_loc.loc_start.pos_fname)))
        in
        let renamed =
          object
            inherit Ast_traverse.map as super

            method! expression expression =
              match expression.pexp_desc with
              | Pexp_ident ({ txt = Lident name; _ } as ident) when Names.mem name hidden ->
                  { expression with pexp_desc = Pexp_ident { ident with txt = Lident (name ^ "_" ^ unit) } }
              | _ -> super#expression expression
          end
        in
        let bound hoisted =
          match hoisted.pstr_desc with
          | Pstr_value (flag, [ ({ pvb_pat = { ppat_desc = Ppat_var ({ txt = name; _ } as var); _ } as pattern; _ } as binding) ])
            when Names.mem name hidden ->
              {
                hoisted with
                pstr_desc =
                  Pstr_value
                    (flag, [ { binding with pvb_pat = { pattern with ppat_desc = Ppat_var { var with txt = name ^ "_" ^ unit } } } ]);
              }
          | _ -> hoisted
        in
        (List.map (fun (hoisted, unused) -> (renamed#structure_item (bound hoisted), unused)) before, renamed#structure_item item)
  in
  let mapped =
    List.map
      (fun item ->
        let saved = !lifted in
        lifted := [];
        let item = pruned#structure_item (map item) in
        let before = List.rev !lifted in
        lifted := saved;
        apart item before)
      items
  in
  fst
    (List.fold_right
       (fun (before, item) (kept, used) ->
         let before, used =
           List.fold_right
             (fun (hoisted, unused) (kept, used) ->
               match (unused, hoisted.pstr_desc) with
               | Dropped, Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; _ } ])
                 when not (Names.mem name used) ->
                   (kept, used)
               | (Dropped | Kept), _ -> (hoisted :: kept, referenced#structure_item hoisted used))
             before ([], used)
         in
         (before @ (item :: kept), used))
       mapped
       ([], List.fold_left (fun used (_, item) -> referenced#structure_item item used) Names.empty mapped))
