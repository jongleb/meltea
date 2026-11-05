let server : View.medium =
  {
    View.browser with
    text =
      (fun [@u] position value ->
        let node = Dom_api.create_text_node Dom_api.document (Option.value ~default:"" value) in
        View.insert position (Dom_api.create_comment Dom_api.document Template.opening);
        View.insert position (Dom_api.to_node node);
        View.insert position (Dom_api.create_comment Dom_api.document Template.closing);
        node);
    root =
      (fun [@u] _ markup ->
        let copy = Template.clone markup in
        let rec mark node =
          Option.iter mark (Dom_api.first_child node);
          let following = Dom_api.next_sibling node in
          (match (Dom_api.node_type node, Dom_api.parent_node node) with
          | 3, Some parent when String.equal (Dom_api.data node) "" ->
              Dom_api.replace_child parent (Dom_api.create_comment Dom_api.document "") node
          | _ -> ());
          Option.iter mark following
        in
        Option.iter mark (Dom_api.first_child (Dom_api.to_node copy));
        copy);
    opened =
      (fun [@u] position ->
        let closing = Dom_api.create_comment Dom_api.document Template.closing in
        View.insert position (Dom_api.create_comment Dom_api.document Template.opening);
        View.insert position closing;
        View.Closing_at closing);
    closed = (fun [@u] _ closing -> View.Closing_at closing);
    aside = (fun [@u] _ -> View.nothing);
  }

let mismatch () = Js.Exn.raiseError "melhole: the HTML from the server does not match the view"

let preceding (position : Template.position) =
  match Js.Null.toOption position.before with
  | Some before -> Dom_api.previous_sibling before
  | None -> Dom_api.last_child position.parent

let rec adopting : View.medium =
  {
    text =
      (fun [@u] position value ->
        match Template.marker (preceding position) Template.closing with
        | None -> mismatch ()
        | Some closing ->
            let node =
              match Js.Null.toOption (Dom_api.previous_sibling closing) with
              | Some found when Dom_api.node_type found = 3 -> Dom_api.unsafe_text found
              | Some _ | None ->
                  let made = Dom_api.create_text_node Dom_api.document "" in
                  Dom_api.insert_before position.parent made (Js.Null.return closing);
                  made
            in
            (match Template.marker (Dom_api.previous_sibling (Dom_api.to_node node)) Template.opening with
            | Some opening -> Dom_api.remove_child position.parent opening
            | None -> mismatch ());
            Dom_api.remove_child position.parent closing;
            Option.iter
              (fun value ->
                if not (String.equal (Dom_api.data (Dom_api.to_node node)) value) then Dom_api.set_data node value)
              value;
            node);
    root =
      (fun [@u] position markup ->
        match Js.Null.toOption (preceding position) with
        | Some node when Dom_api.node_type node = 1 && String.equal (Dom_api.node_name node) (Template.tag markup) ->
            let rec unmark node =
              Option.iter unmark (Dom_api.first_child node);
              let following = Dom_api.next_sibling node in
              (match (Template.marker (Js.Null.return node) "", Dom_api.parent_node node) with
              | Some anchor, Some parent ->
                  Dom_api.replace_child parent (Dom_api.create_text_node Dom_api.document "") anchor
              | Some _, None | None, _ -> ());
              Option.iter unmark following
            in
            Option.iter unmark (Dom_api.first_child node);
            Dom_api.unsafe_element node
        | Some _ | None -> mismatch ());
    placed = (fun [@u] _ _ -> ());
    opened =
      (fun [@u] position ->
        match Template.marker (preceding position) Template.closing with
        | Some closing -> View.Closing_at closing
        | None -> mismatch ());
    closed =
      (fun [@u] parent closing ->
        let rec opening node depth =
          match
            ( Template.marker (Dom_api.previous_sibling node) Template.opening,
              Template.marker (Dom_api.previous_sibling node) Template.closing )
          with
          | Some found, _ when depth = 0 -> found
          | Some found, _ -> opening found (depth - 1)
          | None, Some found -> opening found (depth + 1)
          | None, None -> opening (Js.Null.getUnsafe (Dom_api.previous_sibling node)) depth
        in
        Dom_api.remove_child parent (opening closing 0);
        Dom_api.remove_child parent closing;
        View.Settled);
    built = (fun [@u] count position make -> View.backwards count position make);
    aside =
      (fun [@u] build ->
        View.medium := View.browser;
        let live = build () [@u] in
        View.medium := adopting;
        live);
  }

let render child =
  View.medium := server;
  let root = Dom_api.create_element Dom_api.document "div" in
  match View.mount child ~parent:root ~before:None with
  | mounted ->
      let html = Dom_api.inner_html root in
      View.medium := View.browser;
      View.unmount mounted;
      html
  | exception error ->
      View.medium := View.browser;
      raise error

let hydrate child ~parent =
  View.medium := adopting;
  match View.mount child ~parent ~before:None with
  | mounted ->
      View.medium := View.browser;
      mounted
  | exception _ ->
      View.medium := View.browser;
      Dom_api.set_text_content (Dom_api.to_node parent) "";
      View.mount child ~parent ~before:None

