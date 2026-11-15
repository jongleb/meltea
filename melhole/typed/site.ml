open Ppxlib

(** How a site is decided once the file is typed. *)
type decision =
  | Expression of (File.t -> expression -> expression)
  | Items of (File.t -> structure -> structure)
  | Choice of (File.t -> expression list -> expression option)

let name = "melhole.site"
let among = "melhole.among"
let sites = Hashtbl.create 64

let mark loc decision =
  let number = Hashtbl.length sites + 1 in
  Hashtbl.replace sites number decision;
  Mark.make loc name number

let expression expression ~decide =
  match Mode.current () with
  | Mode.Untyped -> expression
  | Mode.Typed -> { expression with pexp_attributes = mark expression.pexp_loc (Expression decide) :: expression.pexp_attributes }

let choice expression ~among:alternatives ~decide =
  match Mode.current () with
  | Mode.Untyped -> expression
  | Mode.Typed ->
      let loc = expression.pexp_loc in
      let keys = List.map fst alternatives in
      let chosen file held =
        Option.bind (decide file) (fun key ->
            match List.combine keys held with
            | exception Invalid_argument _ -> None
            | paired -> List.assoc_opt key paired)
      in
      {
        expression with
        pexp_attributes =
          mark loc (Choice chosen)
          :: Ast_builder.Default.attribute ~loc ~name:{ loc; txt = among }
               ~payload:(PStr (List.map (fun (_, alternative) -> Ast_builder.Default.pstr_eval ~loc alternative []) alternatives))
          :: expression.pexp_attributes;
      }

let items structure ~decide =
  match (Mode.current (), structure) with
  | Mode.Untyped, _ | Mode.Typed, [] -> structure
  | Mode.Typed, first :: _ ->
      let loc = first.pstr_loc in
      [
        Ast_builder.Default.pstr_include ~loc
          {
            (Ast_builder.Default.include_infos ~loc (Ast_builder.Default.pmod_structure ~loc structure)) with
            pincl_attributes = [ mark loc (Items decide) ];
          };
      ]

let settle structure =
  let lifted = "__melhole_" in
  let names =
    object
      inherit [string list] Ast_traverse.fold as super

      method! longident longident found =
        super#longident longident
          (match Longident.last_exn longident with
          | name when String.starts_with ~prefix:lifted name -> name :: found
          | _ -> found)
    end
  in
  let orphaned = ref [] in
  let decided file =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        let expression = super#expression expression in
        let held =
          List.concat_map
            (fun attribute ->
              match (String.equal attribute.attr_name.txt among, attribute.attr_payload) with
              | true, PStr items ->
                  List.filter_map
                    (fun item -> match item.pstr_desc with Pstr_eval (alternative, _) -> Some alternative | _ -> None)
                    items
              | _ -> [])
            expression.pexp_attributes
        in
        orphaned := List.fold_left (fun found alternative -> names#expression alternative found) !orphaned held;
        let untyped =
          {
            expression with
            pexp_attributes = Mark.without among (Mark.without name (Question.unmarked expression.pexp_attributes));
          }
        in
        match Option.bind (Mark.find name expression.pexp_attributes) (Hashtbl.find_opt sites) with
        | Some (Expression decide) -> Option.fold ~none:untyped ~some:(fun file -> decide file untyped) file
        | Some (Choice decide) ->
            Option.value ~default:untyped (Option.bind file (fun file -> decide file held))
        | Some (Items _) | None -> untyped

      method! value_description description =
        super#value_description { description with pval_attributes = Question.unmarked description.pval_attributes }

      method! structure items =
        List.concat_map
          (fun item ->
            let item = super#structure_item item in
            match item.pstr_desc with
            | Pstr_include { pincl_mod = { pmod_desc = Pmod_structure inner; _ }; pincl_attributes; _ } -> (
                match Mark.find name pincl_attributes with
                | None -> [ item ]
                | Some number -> (
                    match Hashtbl.find_opt sites number with
                    | Some (Items decide) -> Option.fold ~none:inner ~some:(fun file -> decide file inner) file
                    | Some (Expression _ | Choice _) | None -> inner))
            | _ -> [ item ])
          items
    end
  in
  let rec pruned candidates structure =
    let used = names#structure structure [] in
    let unused item =
      match item.pstr_desc with
      | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; pvb_expr; _ } ])
        when List.mem txt candidates && not (List.mem txt used) ->
          Some pvb_expr
      | _ -> None
    in
    let sweeper =
      object
        inherit [string list] Ast_traverse.fold_map as super

        method! structure items dropped =
          let kept, dropped =
            List.fold_right
              (fun item (kept, dropped) ->
                Option.fold ~none:(item :: kept, dropped)
                  ~some:(fun definition -> (kept, names#expression definition dropped))
                  (unused item))
              items ([], dropped)
          in
          super#structure kept dropped
      end
    in
    match sweeper#structure structure [] with swept, [] -> swept | swept, more -> pruned (more @ candidates) swept
  in
  match (Mode.current (), Hashtbl.length sites) with
  | Mode.Untyped, _ -> structure
  | Mode.Typed, 0 -> (decided None)#structure structure
  | Mode.Typed, _ ->
      let settled = (decided (File.of_structure structure))#structure structure in
      Hashtbl.reset sites;
      let candidates = !orphaned in
      orphaned := [];
      (match candidates with [] -> settled | _ :: _ -> pruned candidates settled)

let () = Driver.register_transformation "melhole-typed" ~instrument:(Driver.Instrument.make ~position:After settle)
