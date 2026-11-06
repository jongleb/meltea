open Ppxlib

type t = {
  typed : Ocaml_common.Typedtree.structure;
  env : Ocaml_common.Env.t;
  answers : (Question.t, Answer.t) Hashtbl.t;
}

let of_structure structure =
  let typable =
    object
      inherit Ast_traverse.map as super

      method! structure_item item =
        match item.pstr_desc with
        | Pstr_primitive { pval_type = { ptyp_desc = Ptyp_arrow _; _ }; _ } -> item
        | Pstr_primitive description ->
            let loc = item.pstr_loc in
            Ast_builder.Default.pstr_value ~loc Nonrecursive
              [
                {
                  (Ast_builder.Default.value_binding ~loc
                     ~pat:
                       (Ast_builder.Default.ppat_constraint ~loc
                          (Ast_builder.Default.pvar ~loc description.pval_name.txt)
                          description.pval_type)
                     ~expr:[%expr assert false])
                  with
                  pvb_attributes = description.pval_attributes;
                };
              ]
        | _ -> super#structure_item item

      method! signature_item item =
        match item.psig_desc with
        | Psig_value ({ pval_type = { ptyp_desc = Ptyp_arrow _; _ }; _ }) -> item
        | Psig_value description -> { item with psig_desc = Psig_value { description with pval_prim = [] } }
        | _ -> super#signature_item item
    end
  in
  ignore (Ocaml_common.Warnings.parse_options false "-a");
  ignore (Ocaml_common.Warnings.parse_alert_option "-all");
  (match Ocaml_common.Load_path.get_path_list () with [] -> Ocaml_common.Compmisc.init_path () | _ :: _ -> ());
  match
    Ocaml_common.Typemod.type_structure
      (Ocaml_common.Compmisc.initial_env ())
      (Ppxlib_ast.Selected_ast.to_ocaml Structure (typable#structure structure))
  with
  | exception _ -> None
  | typed, _, _, _, env ->
      let found = ref [] in
      let note attributes answer =
        Option.iter (fun question -> found := (question, answer) :: !found) (Question.of_attributes attributes)
      in
      let iterator =
        {
          Ocaml_common.Tast_iterator.default_iterator with
          expr =
            (fun self (expression : Ocaml_common.Typedtree.expression) ->
              let answer = Answer.make expression.exp_env expression.exp_type in
              note expression.exp_attributes answer;
              List.iter (fun (_, _, attributes) -> note attributes answer) expression.exp_extra;
              Ocaml_common.Tast_iterator.default_iterator.expr self expression);
          value_binding =
            (fun self (binding : Ocaml_common.Typedtree.value_binding) ->
              note binding.vb_attributes (Answer.make binding.vb_pat.pat_env binding.vb_pat.pat_type);
              Ocaml_common.Tast_iterator.default_iterator.value_binding self binding);
          structure_item =
            (fun self (item : Ocaml_common.Typedtree.structure_item) ->
              (match item.str_desc with
              | Ocaml_common.Typedtree.Tstr_primitive description ->
                  note description.val_attributes (Answer.make item.str_env description.val_val.val_type)
              | _ -> ());
              Ocaml_common.Tast_iterator.default_iterator.structure_item self item);
        }
      in
      iterator.structure iterator typed;
      let marked = Hashtbl.create 64 and answers = Hashtbl.create 64 in
      List.iter
        (fun (question, answer) ->
          Hashtbl.replace marked question (1 + Option.value ~default:0 (Hashtbl.find_opt marked question));
          Hashtbl.replace answers question answer)
        !found;
      Hashtbl.iter (fun question count -> match count with 1 -> () | _ -> Hashtbl.remove answers question) marked;
      Some { typed; env; answers }

let typed file = file.typed
let env file = file.env
let answer file question = Hashtbl.find_opt file.answers question
let answers file = List.of_seq (Hashtbl.to_seq file.answers)
