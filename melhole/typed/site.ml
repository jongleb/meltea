open Ppxlib

(** How a site is decided once the file is typed. *)
type decision =
  | Expression of (File.t -> expression -> expression)
  | Items of (File.t -> structure -> structure)

let name = "melhole.site"
let sites = Hashtbl.create 64

let mark loc decision =
  let number = Hashtbl.length sites + 1 in
  Hashtbl.replace sites number decision;
  Mark.make loc name number

let expression expression ~decide =
  match Mode.current () with
  | Mode.Untyped -> expression
  | Mode.Typed -> { expression with pexp_attributes = mark expression.pexp_loc (Expression decide) :: expression.pexp_attributes }

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
  let decided file =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        let expression = super#expression expression in
        let untyped =
          { expression with pexp_attributes = Mark.without name (Question.unmarked expression.pexp_attributes) }
        in
        match Option.bind (Mark.find name expression.pexp_attributes) (Hashtbl.find_opt sites) with
        | Some (Expression decide) -> Option.fold ~none:untyped ~some:(fun file -> decide file untyped) file
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
                    | Some (Expression _) | None -> inner))
            | _ -> [ item ])
          items
    end
  in
  match (Mode.current (), Hashtbl.length sites) with
  | Mode.Untyped, _ -> structure
  | Mode.Typed, 0 -> (decided None)#structure structure
  | Mode.Typed, _ ->
      let settled = (decided (File.of_structure structure))#structure structure in
      Hashtbl.reset sites;
      settled

let () = Driver.register_transformation "melhole-typed" ~instrument:(Driver.Instrument.make ~position:After settle)
