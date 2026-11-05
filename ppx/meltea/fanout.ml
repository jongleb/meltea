open Ppxlib
open Ast_builder.Default

(** A list in the view, and the model fields its items and rows read. *)
type loop = { at : location; reads : Fields.fields }

(** A handler of a frequent event, and the messages it can send. *)
type handler = { site : location; event : string; sends : string list }

let reads ~model env expression =
  let reader =
    object
      inherit [Fields.fields] Ast_traverse.fold as super

      method! expression expression found =
        match expression.pexp_desc with
        | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, { txt; _ })
          when Option.fold ~none:false ~some:(String.equal name) model ->
            Fields.union found (Named [ Longident.last_exn txt ])
        | Pexp_ident { txt = Lident name; _ } when Option.fold ~none:false ~some:(String.equal name) model ->
            Fields.Every
        | Pexp_ident { txt = Lident name; _ } ->
            Fields.union found (Option.value ~default:(Fields.Named []) (List.assoc_opt name env))
        | _ -> super#expression expression found
    end
  in
  reader#expression expression (Named [])

let loops view =
  match view.pexp_desc with
  | Pexp_function ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], _, Pfunction_body body) ->
      let model, env =
        match pattern.ppat_desc with
        | Ppat_var { txt; _ } -> (Some txt, [])
        | Ppat_record (fields, _) ->
            ( None,
              List.concat_map
                (fun ({ txt; _ }, inner) ->
                  List.map (fun name -> (name, Fields.Named [ Longident.last_exn txt ])) (Melhole_compiler.Free.bound inner))
                fields )
        | _ -> (None, [])
      in
      let found = ref [] in
      let rec walk env expression =
        match expression.pexp_desc with
        | Pexp_let (_, bindings, rest) ->
            List.iter (fun binding -> walk env binding.pvb_expr) bindings;
            walk
              (List.concat_map
                 (fun binding ->
                   let read = reads ~model env binding.pvb_expr in
                   List.map (fun name -> (name, read)) (Melhole_compiler.Free.bound binding.pvb_pat))
                 bindings
              @ env)
              rest
        | Pexp_apply
            ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", ("list" | "keyed")); _ }; _ },
              [ (Nolabel, ({ pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "List", "map"); _ }; _ }, _); _ } as items)) ] ) ->
            found := { at = expression.pexp_loc; reads = reads ~model env items } :: !found;
            walk env items
        | _ ->
            let inside =
              object
                inherit Ast_traverse.iter as super

                method! expression child =
                  match child == expression with true -> super#expression child | false -> walk env child
              end
            in
            inside#expression expression
      in
      walk env body;
      List.rev !found
  | _ -> []

let frequent label =
  match label with
  | "onInput" -> Some "input event"
  | "onKeyDown" | "onKeyUp" | "onKeyPress" -> Some "key press"
  | "onMouseMove" | "onPointerMove" | "onTouchMove" -> Some "pointer move"
  | "onScroll" | "onWheel" -> Some "scroll"
  | "onDrag" | "onDragOver" -> Some "drag move"
  | _ -> None

let handlers ~messages items =
  let found = ref [] in
  let finder =
    object
      inherit Ast_traverse.iter as super

      method! expression expression =
        (match (expression.pexp_desc, Melhole_compiler.Element.without_jsx expression.pexp_attributes) with
        | Pexp_apply (_, arguments), Some _ ->
            List.iter
              (fun (label, value) ->
                match label with
                | Labelled name ->
                    Option.iter
                      (fun event ->
                        let rec literal pattern =
                          match pattern.ppat_desc with
                          | Ppat_constant _ -> `Literal
                          | Ppat_or (first, second) -> (
                              match (literal first, literal second) with
                              | `Literal, `Literal -> `Literal
                              | `Any, _ | _, `Any -> `Any)
                          | _ -> `Any
                        in
                        let frequent_part cases =
                          pexp_tuple ~loc:value.pexp_loc
                            (List.filter_map
                               (fun case -> match literal case.pc_lhs with `Literal -> None | `Any -> Some case.pc_rhs)
                               cases)
                        in
                        let sent =
                          match value.pexp_desc with
                          | Pexp_function (_, _, Pfunction_cases (cases, _, _))
                          | Pexp_function (_, _, Pfunction_body { pexp_desc = Pexp_match (_, cases); _ }) ->
                              frequent_part cases
                          | _ -> value
                        in
                        let sends =
                          (object
                             inherit [string list] Ast_traverse.fold as super

                             method! expression expression sent =
                               match expression.pexp_desc with
                               | Pexp_construct ({ txt = Lident name; _ }, _) when List.mem name messages ->
                                   super#expression expression (name :: sent)
                               | _ -> super#expression expression sent
                           end)
                            #expression sent []
                        in
                        match sends with
                        | [] -> ()
                        | _ :: _ -> found := { site = value.pexp_loc; event; sends = List.sort_uniq String.compare sends } :: !found)
                      (frequent name)
                | Nolabel | Optional _ -> ())
              arguments
        | _ -> ());
        super#expression expression
    end
  in
  finder#structure items;
  List.rev !found

let annotate items =
  let binding name =
    List.find_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (_, bindings) ->
            List.find_map
              (fun binding ->
                match binding.pvb_pat.ppat_desc with
                | Ppat_var { txt; _ } when String.equal txt name -> Some binding.pvb_expr
                | _ -> None)
              bindings
        | _ -> None)
      items
  in
  match (Option.bind (binding "update") Fields.branches, Option.map loops (binding "view")) with
  | Some branches, Some (_ :: _ as lists) ->
      let constructors (case : case) =
        let rec names pattern =
          match pattern.ppat_desc with
          | Ppat_construct ({ txt = Lident name; _ }, _) -> `Some [ name ]
          | Ppat_or (first, second) -> (
              match (names first, names second) with
              | `Some first, `Some second -> `Some (first @ second)
              | `All, _ | _, `All -> `All)
          | Ppat_alias (inner, _) | Ppat_constraint (inner, _) -> names inner
          | _ -> `All
        in
        names case.pc_lhs
      in
      let messages =
        List.concat_map (fun (case, _) -> match constructors case with `Some names -> names | `All -> []) branches
      in
      let writes message =
        let rec go branches =
          match branches with
          | [] -> Fields.Named []
          | (case, fields) :: rest -> (
              match (constructors case, case.pc_guard) with
              | `Some names, _ when not (List.mem message names) -> go rest
              | (`Some _ | `All), None -> fields
              | (`Some _ | `All), Some _ -> Fields.union fields (go rest))
        in
        go branches
      in
      let reaching handler =
        List.filter_map
          (fun message ->
            match writes message with
            | Every -> None
            | Named written -> (
                match
                  List.filter_map
                    (fun loop ->
                      match loop.reads with
                      | Every -> None
                      | Named read -> (
                          match List.filter (fun field -> List.mem field read) written with
                          | [] -> None
                          | shared -> Some (loop, shared)))
                    lists
                with
                | [] -> None
                | (loop, shared) :: _ -> Some (message, written, loop, shared)))
          handler.sends
      in
      let alerts =
        List.filter_map
          (fun handler ->
            match reaching handler with
            | [] -> None
            | (message, written, loop, shared) :: _ ->
                Some
                  ( handler.site,
                    Printf.sprintf
                      "each %s here sends %s, which changes %s; the list at line %d reads %s, so every %s checks all of its rows"
                      handler.event message (String.concat ", " written) loop.at.loc_start.pos_lnum
                      (String.concat ", " shared) handler.event ))
          (handlers ~messages items)
      in
      let marker =
        object
          inherit Ast_traverse.map as super

          method! expression expression =
            match List.assoc_opt expression.pexp_loc alerts with
            | Some text ->
                let loc = expression.pexp_loc in
                let here = value_description ~loc ~name:{ txt = "here"; loc } ~type_:[%type: unit] ~prim:[] in
                let alert =
                  attribute ~loc ~name:{ txt = "alert"; loc }
                    ~payload:(PStr [ pstr_eval ~loc (pexp_apply ~loc (evar ~loc "fanout") [ (Nolabel, estring ~loc text) ]) [] ])
                in
                {
                  (pexp_letmodule ~loc
                     { txt = Some "Melhole_fanout"; loc }
                     (pmod_constraint ~loc
                        (pmod_structure ~loc [ [%stri let here = ()] ])
                        (pmty_signature ~loc [ psig_value ~loc { here with pval_attributes = [ alert ] } ]))
                     (pexp_sequence ~loc
                        (pexp_ident ~loc { txt = Ldot (Lident "Melhole_fanout", "here"); loc })
                        (super#expression { expression with pexp_attributes = [] })))
                  with
                  pexp_attributes = expression.pexp_attributes;
                }
            | None -> super#expression expression
        end
      in
      marker#structure items
  | _ -> items
