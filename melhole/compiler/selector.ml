open Ppxlib
open Ast_builder.Default

type equality = Structural | Physical
type t = { chosen : expression; key : expression; equality : equality }

let of_leaf ~outer ~row leaf =
  let loc = leaf.pexp_loc in
  let avoiding names expression =
    match Free.of_expression expression with
    | Reads read when not (List.exists (fun name -> List.mem name names) read) -> Some expression
    | Reads _ | Volatile -> None
  in
  let compared test =
    match test.pexp_desc with
    | Pexp_apply
        ( { pexp_desc = Pexp_ident { txt = Lident (("=" | "==") as operator); _ }; _ },
          [ (Nolabel, left); (Nolabel, right) ] )
      when not (List.mem operator outer) ->
        Some ((match operator with "==" -> Physical | _ -> Structural), left, right)
    | _ -> None
  in
  let rowwise names picked other key =
    Option.bind (avoiding (outer @ names) picked) (fun _ ->
        Option.bind (avoiding (outer @ names) other) (fun _ -> avoiding (outer @ names) key))
  in
  match leaf.pexp_desc with
  | Pexp_match
      ( target,
        [
          { pc_lhs = pattern; pc_guard = Some guard; pc_rhs = picked };
          { pc_lhs = rest; pc_guard = None; pc_rhs = other };
        ] ) -> (
      match (Free.bound pattern, Free.bound rest) with
      | [ bound ], [] ->
          Option.bind (compared guard) (fun (equality, left, right) ->
              let named side =
                match side.pexp_desc with
                | Pexp_ident { txt = Lident name; _ } when String.equal name bound -> Some side
                | _ -> None
              in
              Option.bind
                (match (named left, named right) with
                | Some _, None -> Some right
                | None, Some _ -> Some left
                | Some _, Some _ | None, None -> None)
                (fun key ->
                  Option.bind (avoiding row target) (fun target ->
                      Option.map
                        (fun key ->
                          {
                            chosen =
                              [%expr
                                (match [%e target] with [%p pattern] -> Some [%e evar ~loc bound] | _ -> None)
                                [@ocaml.warning "-11"]];
                            key;
                            equality;
                          })
                        (rowwise [ bound ] picked other key))))
      | _ -> None)
  | Pexp_ifthenelse (test, picked, Some other) ->
      Option.bind (compared test) (fun (equality, left, right) ->
          let ordered chosen key =
            Option.bind (avoiding row chosen) (fun chosen ->
                Option.map (fun key -> { chosen = [%expr Some [%e chosen]]; key; equality }) (rowwise [] picked other key))
          in
          Option.fold ~none:(ordered right left) ~some:Option.some (ordered left right))
  | _ -> None
