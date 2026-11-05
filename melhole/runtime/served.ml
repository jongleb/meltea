let separator = " "

let rec following : View.medium =
  {
    adoption = Server_nodes;
    text =
      (fun [@u] (position : Template.position) value ->
        let at =
          match Js.Null.toOption position.before with
          | Some found when Dom_api.node_type found = 8 && String.equal (Dom_api.data found) separator ->
              Dom_api.following_sibling found
          | Some _ | None -> position.before
        in
        match (Option.value value ~default:"", Js.Null.toOption at) with
        | "", _ ->
            let made = Dom_api.create_text_node Dom_api.document "" in
            Dom_api.insert_before position.parent made position.before;
            made
        | text, Some found when Dom_api.node_type found = 3 ->
            if not (String.equal (Dom_api.data found) text) then Dom_api.set_data (Dom_api.unsafe_text found) text;
            Dom_api.unsafe_text found
        | text, (Some _ | None) -> View.expected ("the text \"" ^ text ^ "\"") at);
    root =
      (fun [@u] (position : Template.position) markup ->
        match Js.Null.toOption position.before with
        | Some found when Dom_api.node_type found = 1 && String.equal (Dom_api.node_name found) (Template.tag markup) ->
            Dom_api.unsafe_element found
        | Some _ | None -> View.expected ("<" ^ String.lowercase_ascii (Template.tag markup) ^ ">") position.before);
    placed = (fun [@u] _ _ -> ());
    opened = (fun [@u] _ -> View.Settled);
    built =
      (fun [@u] count (position : Template.position) make after ->
        match count with
        | 0 -> [||]
        | _ ->
            let first = make 0 position [@u] in
            let made = Array.make count first in
            let at = ref (after first position.before [@u]) in
            for index = 1 to count - 1 do
              let next = make index { position with before = !at } [@u] in
              Array.unsafe_set made index next;
              at := after next !at [@u]
            done;
            made);
    aside =
      (fun [@u] build ->
        View.medium := View.browser;
        let live = build () [@u] in
        View.medium := following;
        live);
  }

let render child =
  let root = Dom_api.create_element Dom_api.document "div" in
  let mounted = View.mount child ~parent:root ~before:None in
  let copy = Dom_api.clone_node root in
  View.unmount mounted;
  let rec tidy node =
    let rec children previous child =
      match Js.Null.toOption child with
      | None -> ()
      | Some current -> (
          let following = Dom_api.following_sibling current in
          match (Dom_api.node_type current, Dom_api.data current, previous) with
          | 8, "", _ | 3, "", _ ->
              Dom_api.remove_child node current;
              children previous following
          | 3, _, `Text ->
              Dom_api.insert_before node (Dom_api.create_comment Dom_api.document separator) (Js.Null.return current);
              children `Text following
          | 3, _, `Other -> children `Text following
          | _, _, (`Text | `Other) ->
              tidy current;
              children `Other following)
    in
    children `Other (Dom_api.first_child_or_end node)
  in
  tidy (Dom_api.to_node copy);
  Dom_api.inner_html copy

let hydrate child ~parent =
  View.medium := following;
  match View.mount_over child ~parent with
  | mounted ->
      View.medium := View.browser;
      Dom_api.queue_microtask (fun () -> Events.replay (Dom_api.to_node parent));
      mounted
  | exception error ->
      Js.Console.error error;
      View.medium := View.browser;
      Dom_api.set_text_content (Dom_api.to_node parent) "";
      View.mount child ~parent ~before:None
