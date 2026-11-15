open Ppxlib
open Ast_builder.Default

(** What a branch of [update] does: leaves the model as it is and runs no command, or anything else. *)
type outcome = Inert | Effective

(** The messages a case of [update] takes: these, or any. *)
type taken = Named of string list | Any

(** A message, where its constructor is written, and the attributes written on it. *)
type message = { name : string; declared : location; silenced : attributes }

let annotate items =
  let built =
    object
      inherit [string list] Ast_traverse.fold as super

      method! expression expression found =
        match expression.pexp_desc with
        | Pexp_construct ({ txt; _ }, _) -> super#expression expression (Longident.last_exn txt :: found)
        | _ -> super#expression expression found
    end
  in
  let everywhere = built#structure items [] in
  let alerts items =
    let update =
      List.find_map
        (fun item ->
          match item.pstr_desc with
          | Pstr_value (_, bindings) ->
              List.find_map
                (fun binding ->
                  match binding.pvb_pat.ppat_desc with
                  | Ppat_var { txt = "update"; _ } -> Some binding.pvb_expr
                  | _ -> None)
                bindings
          | _ -> None)
        items
    in
    let read =
      Option.bind update (fun update ->
          match update.pexp_desc with
          | Pexp_function
              ( [
                  { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = message; _ }; _ }); _ };
                  { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = model; _ }; _ }); _ };
                ],
                None,
                Pfunction_body body ) ->
              let rec dispatch body =
                match body.pexp_desc with
                | Pexp_let (Nonrecursive, bindings, rest)
                  when not
                         (List.exists
                            (fun binding ->
                              List.exists
                                (fun name -> String.equal name message || String.equal name model)
                                (Melhole_compiler.Free.bound binding.pvb_pat))
                            bindings) ->
                    dispatch rest
                | Pexp_match ({ pexp_desc = Pexp_ident { txt = Lident scrutinee; _ }; _ }, cases)
                  when String.equal message scrutinee ->
                    Some (model, cases)
                | _ -> None
              in
              dispatch body
          | _ -> None)
    in
    match read with
    | Some (model, branches) ->
        let rec taken pattern =
          match pattern.ppat_desc with
          | Ppat_construct ({ txt; _ }, _) -> Named [ Longident.last_exn txt ]
          | Ppat_or (first, second) -> (
              match (taken first, taken second) with
              | Named first, Named second -> Named (first @ second)
              | Any, _ | _, Any -> Any)
          | Ppat_alias (inner, _) | Ppat_constraint (inner, _) -> taken inner
          | _ -> Any
        in
        let both first second = match (first, second) with Inert, Inert -> Inert | (Inert | Effective), _ -> Effective in
        let rebinds pattern = List.mem model (Melhole_compiler.Free.bound pattern) in
        let rec outcome expression =
          match expression.pexp_desc with
          | Pexp_ident { txt = Lident name; _ } when String.equal name model -> Inert
          | Pexp_tuple [ state; { pexp_desc = Pexp_ident { txt = Ldot (Lident "Cmd", "none"); _ }; _ } ] -> outcome state
          | Pexp_match (_, cases) when not (List.exists (fun case -> rebinds case.pc_lhs) cases) ->
              List.fold_left (fun found case -> both found (outcome case.pc_rhs)) Inert cases
          | Pexp_ifthenelse (_, yes, Some no) -> both (outcome yes) (outcome no)
          | Pexp_let (_, bindings, body) when not (List.exists (fun binding -> rebinds binding.pvb_pat) bindings) ->
              outcome body
          | Pexp_constraint (inner, _) -> outcome inner
          | _ -> Effective
        in
        let rec arrives name cases =
          match cases with
          | [] -> Effective
          | case :: rest -> (
              match (taken case.pc_lhs, case.pc_guard) with
              | Named names, _ when not (List.mem name names) -> arrives name rest
              | (Named _ | Any), None -> outcome case.pc_rhs
              | (Named _ | Any), Some _ -> both (outcome case.pc_rhs) (arrives name rest))
        in
        let handled =
          List.sort_uniq String.compare
            (List.concat_map (fun case -> match taken case.pc_lhs with Named names -> names | Any -> []) branches)
        in
        let messages =
          Option.value
            ~default:
              (List.filter_map
                 (fun name ->
                   List.find_map
                     (fun case ->
                       match taken case.pc_lhs with
                       | Named names when List.mem name names -> Some { name; declared = case.pc_lhs.ppat_loc; silenced = [] }
                       | Named _ | Any -> None)
                     branches)
                 handled)
            (List.find_map
               (fun item ->
                 match item.pstr_desc with
                 | Pstr_type (_, declarations) ->
                     List.find_map
                       (fun declaration ->
                         match declaration.ptype_kind with
                         | Ptype_variant constructors
                           when handled <> []
                                && List.for_all
                                     (fun name ->
                                       List.exists (fun constructor -> String.equal constructor.pcd_name.txt name) constructors)
                                     handled ->
                             Some
                               (List.map
                                  (fun constructor ->
                                    {
                                      name = constructor.pcd_name.txt;
                                      declared = constructor.pcd_loc;
                                      silenced = constructor.pcd_attributes;
                                    })
                                  constructors)
                         | _ -> None)
                       declarations
                 | _ -> None)
               items)
        in
        let names = List.map (fun message -> message.name) messages in
        let count name found = List.length (List.filter (String.equal name) found) in
        let dead =
          List.filter
            (fun message ->
              count message.name everywhere
              = count message.name
                  (List.concat_map
                     (fun case ->
                       match taken case.pc_lhs with
                       | Named taken when List.mem message.name taken -> built#case case []
                       | Named _ | Any -> [])
                     branches))
            messages
        in
        let idle = ref [] in
        let finder =
          object
            inherit Ast_traverse.iter as super

            method! module_expr _ = ()

            method! expression expression =
              (match (expression.pexp_desc, Melhole_compiler.Element.without_jsx expression.pexp_attributes) with
              | Pexp_apply (_, arguments), Some _ ->
                  List.iter
                    (fun (label, value) ->
                      match label with
                      | Labelled name
                        when String.length name > 2
                             && String.equal (String.sub name 0 2) "on"
                             && Char.equal (Char.uppercase_ascii name.[2]) name.[2] -> (
                          match List.sort_uniq String.compare (List.filter (fun name -> List.mem name names) (built#expression value [])) with
                          | [] -> ()
                          | sends -> (
                              match List.fold_left (fun found name -> both found (arrives name branches)) Inert sends with
                              | Inert -> idle := (value, sends) :: !idle
                              | Effective -> ()))
                      | Labelled _ | Nolabel | Optional _ -> ())
                    arguments
              | _ -> ());
              super#expression expression
          end
        in
        finder#structure items;
        let alert ~loc ~silenced kind text =
          let here = value_description ~loc ~name:{ txt = "here"; loc } ~type_:[%type: unit] ~prim:[] in
          let attribute =
            attribute ~loc ~name:{ txt = "alert"; loc }
              ~payload:(PStr [ pstr_eval ~loc (pexp_apply ~loc (evar ~loc kind) [ (Nolabel, estring ~loc text) ]) [] ])
          in
          [%stri
            let () =
              [%e
                pexp_letmodule ~loc { txt = Some "Melhole_dead"; loc }
                  (pmod_constraint ~loc
                     (pmod_structure ~loc [ [%stri let here = ()] ])
                     (pmty_signature ~loc [ psig_value ~loc { here with pval_attributes = [ attribute ] } ]))
                  {
                    (pexp_ident ~loc { txt = Ldot (Lident "Melhole_dead", "here"); loc }) with
                    pexp_attributes =
                      List.filter
                        (fun attribute -> List.mem attribute.attr_name.txt [ "alert"; "ocaml.alert" ])
                        silenced;
                  }]]
        in
        List.map
          (fun message ->
            alert ~loc:message.declared ~silenced:message.silenced "dead_message"
              (Printf.sprintf
                 "nothing in this file sends %s: no handler, command, subscription or init builds it outside its own branch of update, so that branch runs only if another module sends it"
                 message.name))
          dead
        @ List.rev_map
            (fun (value, sends) ->
              alert ~loc:value.pexp_loc ~silenced:value.pexp_attributes "dead_handler"
                (Printf.sprintf "this handler sends only %s, which %s the model as it is and %s no command: it does nothing"
                   (String.concat " and " sends)
                   (match sends with [ _ ] -> "leaves" | _ -> "leave")
                   (match sends with [ _ ] -> "runs" | _ -> "run")))
            !idle
    | _ -> []
  in
  let rec programs items =
    items
    :: List.concat_map
         (fun item ->
           match item.pstr_desc with
           | Pstr_module { pmb_expr; _ } -> inside pmb_expr
           | Pstr_recmodule bindings -> List.concat_map (fun binding -> inside binding.pmb_expr) bindings
           | _ -> [])
         items
  and inside module_expr =
    match module_expr.pmod_desc with
    | Pmod_structure items -> programs items
    | Pmod_constraint (inner, _) -> inside inner
    | _ -> []
  in
  items @ List.concat_map alerts (programs items)
