open Ppxlib
open Ast_builder.Default

type fields = Every | Named of string list

let union first second =
  match (first, second) with
  | Every, _ | _, Every -> Every
  | Named first, Named second -> Named (List.sort_uniq String.compare (first @ second))

let mask fields =
  match fields with
  | Every -> -1
  | Named names -> List.fold_left (fun mask name -> mask lor (1 lsl (Hashtbl.hash name mod 30))) 0 names

let rec written ~model expression =
  let branches cases =
    List.fold_left
      (fun found case ->
        match List.mem model (Melhole_compiler.Free.bound case.pc_lhs) with
        | true -> Every
        | false -> union found (written ~model case.pc_rhs))
      (Named []) cases
  in
  match expression.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } when String.equal name model -> Named []
  | Pexp_record (fields, Some { pexp_desc = Pexp_ident { txt = Lident base; _ }; _ }) when String.equal base model ->
      Named (List.sort_uniq String.compare (List.map (fun ({ txt; _ }, _) -> Longident.last_exn txt) fields))
  | Pexp_tuple (first :: _) -> written ~model first
  | Pexp_match (_, cases) -> branches cases
  | Pexp_ifthenelse (_, yes, Some no) -> union (written ~model yes) (written ~model no)
  | Pexp_let (_, bindings, body) -> (
      match List.exists (fun binding -> List.mem model (Melhole_compiler.Free.bound binding.pvb_pat)) bindings with
      | true -> Every
      | false -> written ~model body)
  | Pexp_sequence (_, rest) | Pexp_constraint (rest, _) -> written ~model rest
  | _ -> Every

let branches expression =
  let rec dispatch ~message ~model body =
    match body.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, rest)
      when not
             (List.exists
                (fun binding ->
                  List.exists
                    (fun name -> String.equal name message || String.equal name model)
                    (Melhole_compiler.Free.bound binding.pvb_pat))
                bindings) ->
        dispatch ~message ~model rest
    | Pexp_match ({ pexp_desc = Pexp_ident { txt = Lident scrutinee; _ }; _ }, cases)
      when String.equal message scrutinee ->
        Some (List.map (fun case -> (case, written ~model case.pc_rhs)) cases)
    | _ -> None
  in
  match expression.pexp_desc with
  | Pexp_function
      ( [
          { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = message; _ }; _ }); _ };
          { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = model; _ }; _ }); _ };
        ],
        None,
        Pfunction_body body ) ->
      dispatch ~message ~model body
  | _ -> None

let update ~loc expression =
  Option.map
    (fun branches ->
      let rec covering branches =
        match branches with
        | (case, fields) :: later ->
            let mask =
              Option.fold ~none:(mask fields)
                ~some:(fun _ -> List.fold_left (fun found (_, fields) -> found lor mask fields) (mask fields) later)
                case.pc_guard
            in
            { case with pc_guard = None; pc_rhs = eint ~loc mask } :: covering later
        | [] -> []
      in
      [%expr
        (fun __melhole_message -> [%e pexp_match ~loc [%expr __melhole_message] (covering branches)])
        [@ocaml.warning "-a"]])
    (branches expression)

let view expression =
  match expression.pexp_desc with
  | Pexp_function ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], _, Pfunction_body body) -> (
      match pattern.ppat_desc with
      | Ppat_record (fields, _) -> Some (mask (Named (List.map (fun ({ txt; _ }, _) -> Longident.last_exn txt) fields)))
      | Ppat_var { txt = model; _ } ->
          let reader =
            object
              inherit [fields] Ast_traverse.fold as super

              method! expression expression found =
                match expression.pexp_desc with
                | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, { txt; _ })
                  when String.equal name model ->
                    union found (Named [ Longident.last_exn txt ])
                | Pexp_ident { txt = Lident name; _ } when String.equal name model -> Every
                | _ -> super#expression expression found

              method! pattern pattern found =
                match List.mem model (Melhole_compiler.Free.bound pattern) with true -> Every | false -> found
            end
          in
          Some (mask (reader#expression body (Named [])))
      | _ -> None)
  | _ -> None
