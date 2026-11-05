open Ppxlib
open Ast_builder.Default

let bit name = 1 lsl (Hashtbl.hash name mod 30)
let everything = -1

let rec changes ~model expression =
  let branches cases =
    List.fold_left
      (fun mask case ->
        match List.mem model (Melhole_compiler.Free.bound case.pc_lhs) with
        | true -> everything
        | false -> mask lor changes ~model case.pc_rhs)
      0 cases
  in
  match expression.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } when String.equal name model -> 0
  | Pexp_record (fields, Some { pexp_desc = Pexp_ident { txt = Lident base; _ }; _ })
    when String.equal base model ->
      List.fold_left (fun mask ({ txt; _ }, _) -> mask lor bit (Longident.last_exn txt)) 0 fields
  | Pexp_tuple (first :: _) -> changes ~model first
  | Pexp_match (_, cases) -> branches cases
  | Pexp_ifthenelse (_, yes, Some no) -> changes ~model yes lor changes ~model no
  | Pexp_let (_, bindings, body) -> (
      match
        List.exists
          (fun binding -> List.mem model (Melhole_compiler.Free.bound binding.pvb_pat))
          bindings
      with
      | true -> everything
      | false -> changes ~model body)
  | Pexp_sequence (_, rest) | Pexp_constraint (rest, _) -> changes ~model rest
  | _ -> everything

let update ~loc expression =
  match expression.pexp_desc with
  | Pexp_function
      ( [
          { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = message; _ }; _ }); _ };
          { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = model; _ }; _ }); _ };
        ],
        None,
        Pfunction_body
          { pexp_desc = Pexp_match ({ pexp_desc = Pexp_ident { txt = Lident scrutinee; _ }; _ }, cases); _ }
      )
    when String.equal message scrutinee ->
      let masks = List.map (fun case -> changes ~model case.pc_rhs) cases in
      let rec covering cases masks =
        match (cases, masks) with
        | case :: rest, mask :: later ->
            let mask =
              Option.fold ~none:mask ~some:(fun _ -> List.fold_left ( lor ) mask later) case.pc_guard
            in
            { case with pc_guard = None; pc_rhs = eint ~loc mask } :: covering rest later
        | _, _ -> []
      in
      Some
        [%expr
          (fun __melhole_message -> [%e pexp_match ~loc [%expr __melhole_message] (covering cases masks)])
          [@ocaml.warning "-a"]]
  | _ -> None

let view expression =
  match expression.pexp_desc with
  | Pexp_function ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], _, Pfunction_body body) -> (
      match pattern.ppat_desc with
      | Ppat_record (fields, _) ->
          Some (List.fold_left (fun mask ({ txt; _ }, _) -> mask lor bit (Longident.last_exn txt)) 0 fields)
      | Ppat_var { txt = model; _ } ->
          let reader =
            object
              inherit [int] Ast_traverse.fold as super

              method! expression expression mask =
                match expression.pexp_desc with
                | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, { txt; _ })
                  when String.equal name model ->
                    mask lor bit (Longident.last_exn txt)
                | Pexp_ident { txt = Lident name; _ } when String.equal name model -> everything
                | _ -> super#expression expression mask

              method! pattern pattern mask =
                match List.mem model (Melhole_compiler.Free.bound pattern) with
                | true -> everything
                | false -> mask
            end
          in
          Some (reader#expression body 0)
      | _ -> None)
  | _ -> None
