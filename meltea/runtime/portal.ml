external element_by_id : Dom.document -> string -> Dom.element Js.null = "getElementById" [@@mel.send]

(** Where a portal's children are: in the element it names, or in its own place when there is none. *)
type place = Elsewhere of Dom.element | In_place

(** A portal's children, the id it names and where they ended up. *)
type shown = { into : string; place : place; mutable inner : View.live }

let portaled = ref None

let elsewhere into child =
  let hosting =
    match !portaled with
    | Some hosting -> hosting
    | None ->
        let hosting =
          View.hosting
            ~visit:(fun [@u] shown see -> match shown.place with Elsewhere _ -> () | In_place -> View.visit shown.inner see)
            ~first:(fun [@u] shown after ->
              match shown.place with Elsewhere _ -> after | In_place -> View.front shown.inner after)
            ~let_go:(fun [@u] shown ->
              (match shown.place with
              | Elsewhere container ->
                  View.visit shown.inner (fun [@u] node ->
                      Melhole.Dom_api.remove_child (Melhole.Dom_api.to_node container) node)
              | In_place -> ());
              View.release shown.inner)
        in
        portaled := Some hosting;
        hosting
  in
  let fresh build =
    let served = !View.medium in
    View.medium := View.browser;
    let inner = build () in
    View.medium := served;
    inner
  in
  let inside place (position : Melhole.Template.position) =
    match place with
    | Elsewhere container -> { Melhole.Template.parent = Melhole.Dom_api.to_node container; before = Js.null }
    | In_place -> position
  in
  View.host
    ~build:(fun [@u] position ->
      let place =
        match Js.Null.toOption (element_by_id Melhole.Dom_api.document into) with
        | Some container -> Elsewhere container
        | None -> In_place
      in
      View.live hosting { into; place; inner = fresh (fun () -> View.build child (inside place position)) })
    ~patch:(fun [@u] position live ->
      match View.hosted hosting live with
      | Some shown when String.equal shown.into into ->
          shown.inner <- fresh (fun () -> View.update (inside shown.place position) shown.inner child);
          Some live
      | Some _ | None -> None)
