open Ppxlib

type t = Reads of string list | Volatile

exception Unknown

let rec bound pattern =
  match pattern.ppat_desc with
  | Ppat_var { txt; _ } -> [ txt ]
  | Ppat_alias (inner, { txt; _ }) -> txt :: bound inner
  | Ppat_tuple items | Ppat_array items -> List.concat_map bound items
  | Ppat_construct (_, Some (_, inner))
  | Ppat_variant (_, Some inner)
  | Ppat_constraint (inner, _)
  | Ppat_lazy inner
  | Ppat_exception inner
  | Ppat_open (_, inner) ->
      bound inner
  | Ppat_record (fields, _) -> List.concat_map (fun (_, inner) -> bound inner) fields
  | Ppat_or (left, _) -> bound left
  | Ppat_any | Ppat_constant _ | Ppat_interval _ | Ppat_construct (_, None)
  | Ppat_variant (_, None) | Ppat_type _ | Ppat_unpack _ | Ppat_extension _ ->
      []

let rec reads scope expression =
  let within names = reads (names @ scope) in
  let cases scope cases =
    List.concat_map
      (fun case ->
        let scope = bound case.pc_lhs @ scope in
        Option.fold ~none:[] ~some:(reads scope) case.pc_guard @ reads scope case.pc_rhs)
      cases
  in
  match expression.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> if List.mem name scope then [] else [ name ]
  | Pexp_ident
      {
        txt =
          ( Ldot (Ldot (Lident "Js", ("Date" | "Math")), _)
          | Ldot (Lident ("Random" | "Sys" | "Unix"), _) );
        _;
      } ->
      raise Unknown
  | Pexp_ident _ | Pexp_constant _ | Pexp_unreachable -> []
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident ("!" | ":=" | "incr" | "decr"); _ }; _ }, _) ->
      raise Unknown
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "##"; _ }; _ }, [ (_, target); _ ]) ->
      reads scope target
  | Pexp_apply (callee, arguments) ->
      reads scope callee @ List.concat_map (fun (_, argument) -> reads scope argument) arguments
  | Pexp_field (_, { txt = Lident ("contents" | "current") | Ldot (_, ("contents" | "current")); _ })
    ->
      raise Unknown
  | Pexp_field (target, _) -> reads scope target
  | Pexp_let (flag, bindings, body) ->
      let names = List.concat_map (fun binding -> bound binding.pvb_pat) bindings in
      let inner = match flag with Recursive -> names @ scope | Nonrecursive -> scope in
      List.concat_map (fun binding -> reads inner binding.pvb_expr) bindings @ within names body
  | Pexp_function (params, _, body) ->
      let scope, defaults =
        List.fold_left
          (fun (scope, collected) param ->
            match param.pparam_desc with
            | Pparam_val (_, default, pattern) ->
                (bound pattern @ scope, Option.fold ~none:[] ~some:(reads scope) default @ collected)
            | Pparam_newtype _ -> (scope, collected))
          (scope, []) params
      in
      defaults
      @ (match body with
        | Pfunction_body body -> reads scope body
        | Pfunction_cases (body, _, _) -> cases scope body)
  | Pexp_match (target, branches) | Pexp_try (target, branches) ->
      reads scope target @ cases scope branches
  | Pexp_tuple items | Pexp_array items -> List.concat_map (reads scope) items
  | Pexp_construct (_, argument) | Pexp_variant (_, argument) ->
      Option.fold ~none:[] ~some:(reads scope) argument
  | Pexp_record (fields, base) ->
      List.concat_map (fun (_, value) -> reads scope value) fields
      @ Option.fold ~none:[] ~some:(reads scope) base
  | Pexp_ifthenelse (test, yes, no) ->
      reads scope test @ reads scope yes @ Option.fold ~none:[] ~some:(reads scope) no
  | Pexp_sequence (first, second) -> reads scope first @ reads scope second
  | Pexp_constraint (inner, _)
  | Pexp_coerce (inner, _, _)
  | Pexp_lazy inner
  | Pexp_assert inner
  | Pexp_newtype (_, inner)
  | Pexp_poly (inner, _) ->
      reads scope inner
  | Pexp_setfield _ | Pexp_while _ | Pexp_for _ | Pexp_send _ | Pexp_new _
  | Pexp_setinstvar _ | Pexp_override _ | Pexp_letmodule _ | Pexp_letexception _
  | Pexp_object _ | Pexp_pack _ | Pexp_open _ | Pexp_letop _ | Pexp_extension _ ->
      raise Unknown

let of_expression expression =
  match reads [] expression with
  | names -> Reads (List.sort_uniq String.compare names)
  | exception Unknown -> Volatile

let closure ~definitions ~locals expression =
  let rec expand visited pending collected =
    match pending with
    | [] -> Reads (List.sort_uniq String.compare collected)
    | name :: rest when List.mem name visited -> expand visited rest collected
    | name :: rest -> (
        match List.assoc_opt name definitions with
        | None ->
            expand (name :: visited) rest
              (if List.mem name locals then name :: collected else collected)
        | Some definition -> (
            match reads [] definition with
            | more -> expand (name :: visited) (more @ rest) collected
            | exception Unknown -> Volatile))
  in
  match reads [] expression with
  | names -> expand [] names []
  | exception Unknown -> Volatile

let leaves ~outer ~view body =
  let builds expression =
    let finder =
      object
        inherit [bool] Ast_traverse.fold as super

        method! expression expression found =
          found
          || List.exists (fun attribute -> String.equal attribute.attr_name.txt "JSX") expression.pexp_attributes
          || view expression
          || super#expression expression found
      end
    in
    finder#expression expression false
  in
  let rec visit local expression =
    match of_expression expression with
    | Volatile -> raise Unknown
    | Reads names -> (
        let free = List.filter (fun name -> not (List.mem name local)) names in
        match
          ( List.exists (fun name -> List.mem name outer) free,
            List.exists (fun name -> List.mem name local) names || builds expression )
        with
        | false, _ -> []
        | true, false -> [ expression ]
        | true, true -> inside local expression)
  and inside local expression =
    let cases local branches =
      List.concat_map
        (fun case ->
          let local = bound case.pc_lhs @ local in
          Option.fold ~none:[] ~some:(visit local) case.pc_guard @ visit local case.pc_rhs)
        branches
    in
    match expression.pexp_desc with
    | Pexp_let (flag, bindings, rest) ->
        let names = List.concat_map (fun binding -> bound binding.pvb_pat) bindings in
        let inner = match flag with Recursive -> names @ local | Nonrecursive -> local in
        List.concat_map (fun binding -> visit inner binding.pvb_expr) bindings @ visit (names @ local) rest
    | Pexp_function (params, _, body) -> (
        let local =
          List.fold_left
            (fun local param ->
              match param.pparam_desc with
              | Pparam_val (_, _, pattern) -> bound pattern @ local
              | Pparam_newtype _ -> local)
            local params
        in
        match body with Pfunction_body body -> visit local body | Pfunction_cases (branches, _, _) -> cases local branches)
    | Pexp_match (target, branches) | Pexp_try (target, branches) -> visit local target @ cases local branches
    | Pexp_apply (callee, arguments) ->
        visit local callee @ List.concat_map (fun (_, argument) -> visit local argument) arguments
    | Pexp_tuple items | Pexp_array items -> List.concat_map (visit local) items
    | Pexp_construct (_, argument) | Pexp_variant (_, argument) ->
        Option.fold ~none:[] ~some:(visit local) argument
    | Pexp_record (fields, base) ->
        List.concat_map (fun (_, value) -> visit local value) fields @ Option.fold ~none:[] ~some:(visit local) base
    | Pexp_ifthenelse (test, yes, no) ->
        visit local test @ visit local yes @ Option.fold ~none:[] ~some:(visit local) no
    | Pexp_sequence (first, second) -> visit local first @ visit local second
    | Pexp_field (target, _) | Pexp_constraint (target, _) | Pexp_coerce (target, _, _) -> visit local target
    | _ -> raise Unknown
  in
  match visit [] body with leaves -> Some leaves | exception Unknown -> None
