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

type use = Fields of longident loc list | Whole

let use variable expression =
  (object
     inherit [use] Ast_traverse.fold as super

     method! expression expression found =
       match (found, expression.pexp_desc) with
       | Whole, _ -> Whole
       | Fields fields, Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, field) when String.equal name variable ->
           Fields (match List.exists (fun known -> known.txt = field.txt) fields with true -> fields | false -> field :: fields)
       | Fields _, Pexp_ident { txt = Lident name; _ } when String.equal name variable -> Whole
       | Fields _, _ -> super#expression expression found

     method! pattern pattern found =
       match pattern.ppat_desc with
       | (Ppat_var { txt = name; _ } | Ppat_alias (_, { txt = name; _ })) when String.equal name variable -> Whole
       | _ -> super#pattern pattern found
  end)
    #expression expression (Fields [])

(** When the reads of an expression happen: while it is evaluated, or later, when a function that holds
    it is called. *)
type timing = Now | Later

(** How the body of a function is read: as if evaluated with the function, or as run when it is called. *)
type bodies = With_function | When_called

let rec reads bodies timing scope expression =
  let reads_in = reads bodies timing in
  let within names = reads_in (names @ scope) in
  let later read = match timing with Now -> raise Unknown | Later -> read () in
  let cases scope cases =
    List.concat_map
      (fun case ->
        let scope = bound case.pc_lhs @ scope in
        Option.fold ~none:[] ~some:(reads_in scope) case.pc_guard @ reads_in scope case.pc_rhs)
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
      later (fun () -> [])
  | Pexp_ident _ | Pexp_constant _ | Pexp_unreachable -> []
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident ("!" | ":=" | "incr" | "decr"); _ }; _ }, arguments) ->
      later (fun () -> List.concat_map (fun (_, argument) -> reads_in scope argument) arguments)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "##"; _ }; _ }, [ (_, target); _ ]) ->
      reads_in scope target
  | Pexp_apply (callee, arguments) ->
      reads_in scope callee @ List.concat_map (fun (_, argument) -> reads_in scope argument) arguments
  | Pexp_field (target, { txt = Lident ("contents" | "current") | Ldot (_, ("contents" | "current")); _ })
    ->
      later (fun () -> reads_in scope target)
  | Pexp_field (target, _) -> reads_in scope target
  | Pexp_let (flag, bindings, body) ->
      let names = List.concat_map (fun binding -> bound binding.pvb_pat) bindings in
      let inner = match flag with Recursive -> names @ scope | Nonrecursive -> scope in
      List.concat_map (fun binding -> reads_in inner binding.pvb_expr) bindings @ within names body
  | Pexp_function (params, _, body) ->
      let reads_body = reads bodies (match bodies with With_function -> timing | When_called -> Later) in
      let scope, defaults =
        List.fold_left
          (fun (scope, collected) param ->
            match param.pparam_desc with
            | Pparam_val (_, default, pattern) ->
                (bound pattern @ scope, Option.fold ~none:[] ~some:(reads_body scope) default @ collected)
            | Pparam_newtype _ -> (scope, collected))
          (scope, []) params
      in
      defaults
      @ (match body with
        | Pfunction_body body -> reads_body scope body
        | Pfunction_cases (body, _, _) ->
            List.concat_map
              (fun case ->
                let scope = bound case.pc_lhs @ scope in
                Option.fold ~none:[] ~some:(reads_body scope) case.pc_guard @ reads_body scope case.pc_rhs)
              body)
  | Pexp_match (target, branches) | Pexp_try (target, branches) ->
      reads_in scope target @ cases scope branches
  | Pexp_tuple items | Pexp_array items -> List.concat_map (reads_in scope) items
  | Pexp_construct (_, argument) | Pexp_variant (_, argument) ->
      Option.fold ~none:[] ~some:(reads_in scope) argument
  | Pexp_record (fields, base) ->
      List.concat_map (fun (_, value) -> reads_in scope value) fields
      @ Option.fold ~none:[] ~some:(reads_in scope) base
  | Pexp_ifthenelse (test, yes, no) ->
      reads_in scope test @ reads_in scope yes @ Option.fold ~none:[] ~some:(reads_in scope) no
  | Pexp_sequence (first, second) -> reads_in scope first @ reads_in scope second
  | Pexp_constraint (inner, _)
  | Pexp_coerce (inner, _, _)
  | Pexp_lazy inner
  | Pexp_assert inner
  | Pexp_newtype (_, inner)
  | Pexp_poly (inner, _) ->
      reads_in scope inner
  | Pexp_setfield (target, _, value) -> later (fun () -> reads_in scope target @ reads_in scope value)
  | Pexp_while (test, body) -> later (fun () -> reads_in scope test @ reads_in scope body)
  | Pexp_for (pattern, low, high, _, body) ->
      later (fun () -> reads_in scope low @ reads_in scope high @ within (bound pattern) body)
  | Pexp_send (target, _) -> later (fun () -> reads_in scope target)
  | Pexp_new _ | Pexp_setinstvar _ | Pexp_override _ | Pexp_letmodule _ | Pexp_letexception _
  | Pexp_object _ | Pexp_pack _ | Pexp_open _ | Pexp_letop _ | Pexp_extension _ ->
      raise Unknown

let of_expression expression =
  match reads With_function Now [] expression with
  | names -> Reads (List.sort_uniq String.compare names)
  | exception Unknown -> Volatile

let captured expression =
  match reads When_called Now [] expression with
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
            match reads With_function Now [] definition with
            | more -> expand (name :: visited) (more @ rest) collected
            | exception Unknown -> Volatile))
  in
  match reads With_function Now [] expression with
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
        | true, false -> (
            match expression.pexp_desc with
            | Pexp_function _ ->
                List.map
                  (fun name -> Ast_builder.Default.evar ~loc:expression.pexp_loc name)
                  (List.filter (fun name -> List.mem name outer) free)
            | _ -> [ expression ])
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
