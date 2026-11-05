open Ppxlib

class mapper =
  object (self)
    inherit Ast_traverse.map as super
    val mutable locals : string list = []
    val mutable definitions : (string * expression) list = []
    method locals = locals
    method definitions = definitions

    method private within : 'a. string list -> (unit -> 'a) -> 'a =
      fun names run ->
        let saved = locals and known = definitions in
        locals <- names @ locals;
        definitions <-
          List.filter
            (fun (name, definition) ->
              match Free.of_expression definition with
              | Reads read -> not (List.exists (fun bound -> List.mem bound names) (name :: read))
              | Volatile -> not (List.mem name names))
            definitions;
        match run () with
        | result ->
            locals <- saved;
            definitions <- known;
            result
        | exception error ->
            locals <- saved;
            definitions <- known;
            raise error

    method! case case = self#within (Free.bound case.pc_lhs) (fun () -> super#case case)

    method! expression expression =
      match expression.pexp_desc with
      | Pexp_let (flag, bindings, body) ->
          let names = List.concat_map (fun binding -> Free.bound binding.pvb_pat) bindings in
          let defined =
            match flag with
            | Recursive -> []
            | Nonrecursive ->
                List.filter_map
                  (fun binding ->
                    match binding.pvb_pat.ppat_desc with
                    | Ppat_var { txt; _ } -> Some (txt, binding.pvb_expr)
                    | _ -> None)
                  bindings
          in
          let bindings =
            match flag with
            | Recursive -> self#within names (fun () -> List.map self#value_binding bindings)
            | Nonrecursive -> List.map self#value_binding bindings
          in
          let body =
            self#within names (fun () ->
                let known = definitions in
                definitions <- defined @ definitions;
                match self#expression body with
                | result ->
                    definitions <- known;
                    result
                | exception error ->
                    definitions <- known;
                    raise error)
          in
          { expression with pexp_desc = Pexp_let (flag, bindings, body) }
      | Pexp_function (params, constraint_, body) ->
          let rec go params mapped =
            match params with
            | [] ->
                let body =
                  match body with
                  | Pfunction_body body -> Pfunction_body (self#expression body)
                  | Pfunction_cases (cases, loc, attributes) ->
                      Pfunction_cases (List.map self#case cases, loc, attributes)
                in
                { expression with pexp_desc = Pexp_function (List.rev mapped, constraint_, body) }
            | ({ pparam_desc = Pparam_val (label, default, pattern); _ } as param) :: rest ->
                let param =
                  {
                    param with
                    pparam_desc = Pparam_val (label, Option.map self#expression default, pattern);
                  }
                in
                self#within (Free.bound pattern) (fun () -> go rest (param :: mapped))
            | ({ pparam_desc = Pparam_newtype _; _ } as param) :: rest -> go rest (param :: mapped)
          in
          go params []
      | Pexp_match (target, cases) ->
          { expression with pexp_desc = Pexp_match (self#expression target, List.map self#case cases) }
      | Pexp_try (target, cases) ->
          { expression with pexp_desc = Pexp_try (self#expression target, List.map self#case cases) }
      | Pexp_for (pattern, low, high, direction, body) ->
          let body = self#within (Free.bound pattern) (fun () -> self#expression body) in
          {
            expression with
            pexp_desc =
              Pexp_for (pattern, self#expression low, self#expression high, direction, body);
          }
      | _ -> super#expression expression
  end
