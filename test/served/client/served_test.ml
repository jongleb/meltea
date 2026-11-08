open Testing

external server_page : int array -> string = "page"
[@@mel.module "../../../../../server_js/output/test/served/server_js/rendered.mjs"]

external read_file : string -> string -> string = "readFileSync" [@@mel.module "node:fs"]
external parse_json : string -> 'a = "parse" [@@mel.scope "JSON"]
external set_inner_html : Dom.element -> string -> unit = "innerHTML" [@@mel.set]
external select_all : Dom.element -> string -> Dom.nodeList = "querySelectorAll" [@@mel.send]
external elements : Dom.nodeList -> Dom.element array = "from" [@@mel.scope "Array"]
external remove : Dom.element -> unit = "remove" [@@mel.send]
external value_of : Dom.element -> string = "value" [@@mel.get]
external checked_of : Dom.element -> bool = "checked" [@@mel.get]
external as_element : Dom.node -> Dom.element = "%identity"
external css_text : Dom.element -> string = "cssText" [@@mel.get] [@@mel.scope "style"]
external clone : Dom.element -> (_[@mel.as {json|true|json}]) -> Dom.element = "cloneNode" [@@mel.send]
external set_attribute : Dom.element -> string -> string -> unit = "setAttribute" [@@mel.send]
external js_string : float -> string = "String"
external double : unit -> float arbitrary = "double" [@@mel.module "fast-check"]

let of_bits : string -> float =
  [%mel.raw {|function (bits) { return new Float64Array(new BigInt64Array([BigInt(bits)]).buffer)[0]; }|}]

external stringify : string -> string = "stringify" [@@mel.scope "JSON"]
external parse_string : string -> string = "parse" [@@mel.scope "JSON"]
external full_string : unit -> string arbitrary = "fullUnicodeString" [@@mel.module "fast-check"]

let parsed_page : string -> Dom.document =
  [%mel.raw {|function (html) { return new DOMParser().parseFromString(html, "text/html"); }|}]

external by_id : Dom.document -> string -> Dom.element Js.null = "getElementById" [@@mel.send]
external query_all : Dom.document -> string -> Dom.nodeList = "querySelectorAll" [@@mel.send]
external adopt_node : Dom.document -> Dom.element -> Dom.element = "adoptNode" [@@mel.send]

let fragments = [| "</script>"; "</SCRIPT >"; "<!--"; "-->"; "<script>"; "\u{2028}"; "\\"; "\""; "<"; "&lt;"; "]]>" |]

let corpus () = parse_json (read_file "_build/default/test/served/native/corpus.json" "utf8")

(** A node as the parser builds it: comments and texts kept, attributes by name, style as the browser reads it. *)
type parsed = Node_element of string * (string * string) list * parsed list | Node_text of string | Node_comment of string

let rec parsed ~ignored node =
  match node_type node with
  | 1 ->
      let element = as_element node in
      Node_element
        ( tag_name node,
          List.sort compare
            (List.filter_map
               (fun name ->
                 match String.lowercase_ascii name with
                 | "style" -> Some ("style", css_text element)
                 | lowered when List.mem lowered ignored -> None
                 | lowered -> Some (lowered, attribute node name))
               (Array.to_list (attribute_names node))),
          List.map (parsed ~ignored) (Array.to_list (nodes (node_children node))) )
  | 3 -> Node_text (node_value node)
  | _ -> Node_comment (node_value node)

let page_of html =
  let root = create_element document "div" in
  set_inner_html root html;
  List.map (parsed ~ignored:[ "checked" ]) (Array.to_list (nodes (node_children (as_node root))))

let observe root =
  let copy = clone root in
  Array.iter (fun element -> set_attribute element "style" (css_text element)) (elements (select_all copy "[style]"));
  ( shapes (as_node copy),
    List.map (fun field -> (value_of field, checked_of field)) (Array.to_list (elements (select_all root "input, textarea"))) )

let () =
  describe "Server rendering without a DOM" (fun () ->
      test "a float is written as JavaScript writes it, under Melange" (fun () ->
          check
            (property (double ()) (fun value -> expect (Melhole_server.float value) |. to_be (js_string value)))
            [%mel.obj { numRuns = 3000 }]);
      test "a float is written as JavaScript writes it, natively" (fun () ->
          let floats = (corpus ())##floats in
          expect
            (Array.to_list (Array.map (fun entry -> js_string (of_bits entry##bits)) floats))
          |. to_equal (Array.to_list (Array.map (fun entry -> entry##text) floats)));
      test "data written into the page reads back as it was, and no text in it ends the script" (fun () ->
          check
            (property
               (array_of (full_string ()) [%mel.obj { minLength = 0; maxLength = 6 }])
               (fun pieces ->
                 let text =
                   String.concat ""
                     (Array.to_list
                        (Array.mapi
                           (fun index piece -> piece ^ fragments.((index + String.length piece) mod Array.length fragments))
                           pieces))
                 in
                 let page =
                   parsed_page
                     ("<!doctype html><html><head>" ^ Melhole_server.data ~id:"model" (stringify text)
                    ^ "</head><body><p id=\"after\">ok</p></body></html>")
                 in
                 let script = Js.Null.getExn (by_id page "model") in
                 let after = Array.length (nodes (query_all page "p")) in
                 let scripts = Array.length (nodes (query_all page "script")) in
                 let live = adopt_node document script in
                 append_child (body document) live;
                 let read = Option.map parse_string (Melhole.Served.data ~id:"model") in
                 remove live;
                 expect (read, after, scripts) |. to_equal (Some text, 1, 1)))
            [%mel.obj { numRuns = 500 }]);
      test "native and Melange servers write the same HTML for the same model" (fun () ->
          let pages = (corpus ())##pages in
          expect (Array.to_list (Array.map (fun entry -> server_page entry##tape) pages))
          |. to_equal (Array.to_list (Array.map (fun entry -> entry##html) pages)));
      test "the server's HTML parses into the nodes the DOM renderer writes: texts, separators, elements, attributes" (fun () ->
          check
            (property (tapes ~shortest:0 ~longest:60) (fun values ->
                 expect (page_of (server_page values))
                 |. to_equal (page_of (Program.render ~view:Pages.view (Pages.decode values)))))
            [%mel.obj { numRuns = 300 }]);
      test "the browser hydrates the server's HTML with the server's own elements, then patches like a mounted page" (fun () ->
          check
            (property (tapes ~shortest:0 ~longest:80) (fun values ->
                 let tape = tape values in
                 let model = Pages.decode values in
                 let served = attached () and fresh = attached () in
                 set_inner_html served (server_page values);
                 let sent = nodes (select_all served "*") in
                 let hydrated = Program.sandbox ~start:Program.hydrate ~init:model ~update:Pages.update ~view:Pages.view served in
                 let present = nodes (select_all served "*") in
                 let foreign =
                   Array.length (Js.Array.filter ~f:(fun node -> not (Array.exists (fun other -> same node other) sent)) present)
                   + Array.length (Js.Array.filter ~f:(fun node -> not (Array.exists (fun other -> same node other) present)) sent)
                 in
                 let mounted = Program.sandbox ~init:model ~update:Pages.update ~view:Pages.view fresh in
                 let first = observe served = observe fresh in
                 let messages =
                   List.init (draw tape 6) (fun _ ->
                       match draw tape 4 with
                       | 0 -> Pages.Toggle (draw tape 5)
                       | 1 -> Pages.Type Pages.words.(draw tape (Array.length Pages.words))
                       | 2 -> Pages.Press
                       | _ -> Pages.Pick Pages.words.(draw tape (Array.length Pages.words)))
                 in
                 let agreed =
                   List.for_all
                     (fun message ->
                       Program.dispatch hydrated message;
                       Program.flush hydrated;
                       Program.dispatch mounted message;
                       Program.flush mounted;
                       observe served = observe fresh)
                     messages
                 in
                 Program.stop hydrated;
                 Program.stop mounted;
                 remove served;
                 remove fresh;
                 expect (foreign, first, agreed) |. to_equal (0, true, true)))
            [%mel.obj { numRuns = 300 }]);
      test "hydration writes nothing the server's HTML already shows: no attribute, no text, no element added or taken away"
        (fun () ->
          let rewrites : 'record array -> string array =
            [%mel.raw
              {|function (records) {
  const shown = (node) => node.nodeType === 1 || (node.nodeType === 3 && node.data !== "");
  return records.flatMap((record) =>
    record.type === "attributes" ? ["attribute " + record.attributeName + " of <" + record.target.nodeName + ">"]
    : record.type === "characterData" ? ["text " + JSON.stringify(record.target.data)]
    : [...record.addedNodes, ...record.removedNodes].filter(shown).map((node) => "child " + node.nodeName));
}|}]
          in
          check
            (property (tapes ~shortest:0 ~longest:80) (fun values ->
                 let served = attached () in
                 set_inner_html served (server_page values);
                 let changes = observer (fun () -> ()) in
                 Testing.observe changes served
                   [%mel.obj { childList = true; subtree = true; attributes = true; characterData = true }];
                 let hydrated =
                   Program.sandbox ~start:Program.hydrate ~init:(Pages.decode values) ~update:Pages.update ~view:Pages.view served
                 in
                 let written = rewrites (take_records changes) in
                 disconnect changes;
                 Program.stop hydrated;
                 remove served;
                 expect written |. to_equal [||]))
            [%mel.obj { numRuns = 300 }]))
