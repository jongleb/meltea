<div align="center">

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/meltea-dark.svg">
  <img src="assets/meltea.svg" alt="Meltea: a tea glass with a hole in orbit around it" width="120">
</picture>

# Meltea

**The Elm architecture for OCaml and Melange: pure functions everywhere, views in mlx,
compiled like Svelte.**

</div>

A model, messages, a pure `update`, a pure `view`. No hooks, no components with state, no
virtual DOM. The view is written in mlx and compiled at build time into a DOM template with
holes and a refresh function that writes only what changed.

```ocaml
type message = Increment | Decrement | Reset

let update message count =
  match message with Increment -> count + 1 | Decrement -> count - 1 | Reset -> 0

let view count =
  <div className="counter">
    <button onClick=Decrement>(Html.text "-")</button>
    <output>(Html.int count)</output>
    <button onClick=Increment>(Html.text "+")</button>
    <button onClick=Reset disabled=(count = 0)>(Html.text "reset")</button>
  </div>

let () = Option.iter (fun root -> ignore (Program.sandbox ~init:0 ~update ~view root)) root
```

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/compiled-dark.svg">
  <img src="assets/compiled.svg" alt="The counter in Meltea: you write the view in mlx; Meltea compiles it into a template tree of div, two buttons and an output whose text is the only hole; when + is clicked the update makes count 42, the compiled code compares count with its last value and writes 42 into that one text node; the grey static nodes get no DOM writes; there is no virtual DOM" width="100%">
</picture>

## How it works

- **Templates.** `meltea-ppx` turns the static part of every view into HTML, parsed once into a
  `<template>` and cloned. Every dynamic part is a hole, reached on the clone by a
  `firstChild`/`nextSibling` walk the compiler writes out.
- **Refresh.** For every template the compiler emits the update code, as Svelte and
  [dartea](https://github.com/jongleb/dartea) do. Each local the view reads is checked with one
  `===`; only the holes that read a changed one are evaluated and written. Pure views and
  immutable values make this exact: an unchanged reference is an unchanged value.
- **Fields.** The compiler reads `update` and `view`: which model fields each message can change,
  which fields the view reads. A message that changes nothing the view reads does not call it, and
  `subscriptions` are asked again only when a field they read changed.
- **Remembered values.** `let shown = List.filter … model.items` at the top of a view is computed
  again only when `model.items` or another input changed; otherwise it is the same list, and the
  holes that read it are skipped, so typing in a field does not check the list's rows.

  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/remember-dark.svg">
    <img src="assets/remember.svg" alt="Type m changes only draft; let shown = List.filter … model.items is remembered by model.filter and model.items, so the old list comes back and the list hole is skipped" width="100%">
  </picture>
- **Fanout.** The compiler follows each frequent event (typing, keys, pointer moves, scroll) to the
  message it sends, the fields that message's branch writes and the lists that read them. When
  typing in a field would make a list check all its rows on every keystroke, it says so at the
  handler, as a compiler alert.

  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/fanout-dark.svg">
    <img src="assets/fanout.svg" alt="Fanout in TodoMVC: the edit field's onInput sends Retype, which writes editing, which the list reads, so the compiler raises the alert fanout at the handler; the new-todo field's onInput sends Type, which writes field, which no list reads" width="100%">
  </picture>
- **Lists.** `Html.list (List.map row items)` is a keyed loop with selectors: a row is rendered
  again only when its item is another object or a piece of it that reads outer values changed.
- **Events.** `onClick=Increment` sends a message. `onInput` and `onKeyDown` take a function of
  the value or the key, `onCheck` of the checked state. Handlers are delegated to `document`.
- **Controlled inputs.** `value` and `checked` always show the model: after typing, the field
  gets the model's value back if the update did not take the input.
- **Pacing.** A message sent by a click or a key is drawn at the next microtask; messages sent by
  pointer moves, scroll and wheel are drawn once per frame, as the Elm runtime does, and a click
  in between draws at once.
- **Hot reload.** With `~start:Program.hot` and Vite's HMR, an edit to the code shows the new view
  of the old model; when the module's types changed it starts from `init`.

  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/hot-dark.svg">
    <img src="assets/hot.svg" alt="Hot reload: new code on the same root takes the running model when the shape of the module's types is the same, and starts from init otherwise" width="100%">
  </picture>
- **Effects.** `Cmd` and `Sub` are values that describe effects; `Program` runs them.
- **Server.** `Program.render` writes HTML in React's format, `~start:Program.hydrate` takes it over
  in the browser; compile with `(pps melange.ppx meltea-ppx -hydratable)` to include the code that
  does it. On a server the same module builds against `meltea.server` with `meltea-ppx -server`:
  the view compiles to code that writes strings, with no DOM, natively or in Node.
  `Melhole_server.capture` in the page's head keeps the clicks and typing made before the code
  arrives, and hydration delivers them.
- **Transitions.** `Html.transition ~name` and `Html.transition_group ~name` animate what enters,
  leaves and moves with CSS classes, as Vue's `<Transition>` and `<TransitionGroup>`;
  `Cmd.view_transition` from an update draws inside the browser's View Transitions.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/fields-dark.svg">
  <img src="assets/fields.svg" alt="Fields an update changes, in Meltea's TodoMVC: meltea-ppx reads update and finds what each message can change: Type changes field, Route changes filter, Add changes todos, field and next, Ignore changes nothing, and Edit, which it does not recognise, changes all fields. It reads view the same way: todos, field, editing and filter, never next. After Ignore nothing the view reads changed, so the view is not called; after Type the view runs and one === per local picks the holes to write" width="100%">
</picture>

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/selectors-dark.svg">
  <img src="assets/selectors.svg" alt="Compiled selectors, after Vue Vapor: a row of Html.list (List.map …) reads its item and the outer value selected through one leaf without JSX; the compiler emits a projection that evaluates only that leaf for each row. When selected goes from Some 2 to Some 5 and the six items are the same objects, only rows 2 and 5 have a changed projection, so only they render, in place; the other four are kept without calling the row function" width="100%">
</picture>

The counter's view compiles to this JavaScript (names shortened):

```js
const template = View.template(Template.html("<div class=\"counter\"><button>-</button>…"),
  (scope, div) => {
    const minus = div.firstChild, output = minus.nextSibling, plus = output.nextSibling, …;
    return [handler_hole(scope, minus, "click"), text_hole({ parent: output, before: null }), …];
  });

const view = (count, dispatch) =>
  View.compiled(template, 2, (scope, holes) => {
    const sent = check(scope, 0, dispatch), counted = check(scope, 1, count); // one === each
    if (sent === Changed) set_handler(holes[0], () => dispatch(Decrement));  // first draw only
    if (counted === Changed) set_text(holes[1], String(count));
    …
  });
```

## Transitions

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/transition-dark.svg">
  <img src="assets/transition.svg" alt="Transitions, as Vue's Transition and TransitionGroup, with name fade. Enter: in the patch the new row b gets fade-enter-from and fade-enter-active, two frames later fade-enter-to replaces fade-enter-from while CSS animates, and when the duration is up no classes are left. Leave: the removed row b is put back where it was with fade-leave-from and fade-leave-active, two frames later fade-leave-to, and when the duration is up it leaves the DOM. Move, in a group: row c went first, so after the patch the DOM is c a b but a transform shows every moved row where it was; two frames later the rows get fade-move and the transforms are cleared, so they slide to their new places; at the end no classes are left" width="100%">
</picture>

```ocaml
let view model =
  <ul className="todo-list">
    (Html.transition_group ~name:"fade" (Html.list (List.map row (visible model))))
  </ul>
```

```css
.fade-enter-from, .fade-leave-to { opacity: 0; }
.fade-enter-active, .fade-leave-active, .fade-move { transition: all 200ms; }
```

The wrapper stays mounted while what it shows changes. An element that appears gets
`fade-enter-from` and `fade-enter-active`, then `fade-enter-to` from the next frame; one that goes
stays where it was with the `leave` classes until its transition ends. The time is `~duration`
milliseconds, or the longest delay plus duration in the element's computed style. The first render
does not animate.

## TodoMVC

[`examples/todomvc`](examples/todomvc/todomvc.mlx) is the full [TodoMVC](https://todomvc.com):
routing by `#/active` and `#/completed`, editing with Enter, Escape and blur, `localStorage`
through [jsonkit](https://github.com/melange-community/jsonkit)'s `[@@deriving json]`. A row:

```ocaml
let row editing todo =
  let text = match editing with Editing { id; text } when id = todo.id -> Some text | _ -> None in
  <li key=(string_of_int todo.id) className=(...)>
    <div className="view">
      <input className="toggle" type_="checkbox" checked=(todo.status = Completed)
        onCheck=(fun _ -> Toggle todo.id) />
      <label onDoubleClick=(Edit todo.id)>(Html.text todo.title)</label>
      <button className="destroy" onClick=(Destroy todo.id) />
    </div>
    (Option.fold ~none:Html.none ~some:(fun text -> <input className="edit" value=text ... />) text)
  </li>
```

The update asks for focus with `Cmd.focus`, `main.mlx` saves through `Cmd.port`, and the route
comes from `Sub.hash`. A model-based law drives it with real DOM events (typing, Enter, Escape,
double-click, blur, clicks, hash changes) and checks the page against a plain list of todos
after every step.

## On a server

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/served-dark.svg">
  <img src="assets/served.svg" alt="One Meltea module, page.mlx, built twice. meltea-ppx for the browser compiles the view into a template parsed once and cloned, a walk to the output and a refresh that writes the text only when count changed. meltea-ppx -server compiles the same view into code that writes strings with no DOM, natively or in Node: the static markup joined into a few strings at compile time and one call per dynamic part. The server writes HTML in React's format, the same bytes natively and under Melange, and the browser build hydrates it" width="100%">
</picture>

One module with the model, `update` and `view` builds for both sides
([`examples/served`](../examples/served)):

```
(executable                              ; the server, native
 (name server)
 (libraries meltea.server)
 (preprocess (pps meltea-ppx -server)))

(melange.emit                            ; the browser
 (target app)
 (libraries meltea)
 (preprocess (pps melange.ppx meltea-ppx -hydratable)))
```

```ocaml
(* server: the capture script, the model as data, the page *)
"<script>" ^ Melhole_server.capture ^ "</script>" ^ Melhole_server.data ~id:"model" (encode model)
^ "<div id=\"root\">" ^ Program.render ~view:Page.view model ^ "</div>"

(* browser: start from the server's model, no init and no commands run twice *)
Option.iter
  (fun text -> ignore (Program.sandbox ~start:Program.hydrate ~init:(decode text) ~update:Page.update ~view:Page.view root))
  (Melhole.Served.data ~id:"model")
```

## API

| module | what it gives |
|---|---|
| `Program` | `sandbox ~init ~update ~view root`, `element ~init ~update ~view ~subscriptions root`, `dispatch`, `flush`, `stop`; `render ~view model` for the server and `~start:Program.hydrate` to take its HTML over |
| `Html` | `text`, `int`, `float`, `none`, `list`, `keyed`, `map`, `memo` (Elm's `lazy`), `transition` and `transition_group` (`~name`, `?duration`) |
| `Cmd` | `none`, `batch`, `map`, `message`, `perform` (a promise), `focus` (an element by id, after the next draw), `port` (a value out to JavaScript), `view_transition` (the next draw inside `document.startViewTransition`) |
| `Sub` | `none`, `batch`, `map`, `every`, `hash` (the URL fragment) |

Attributes are the familiar DOM props (`className`, `value`, `checked`, `disabled`, …) with
their OCaml types; a typo is a compile error that names the element.

## Use

```
(melange.emit
 (target app)
 (libraries meltea)
 (preprocess (pps melange.ppx meltea-ppx)))
```

and `(dialect (name mlx) …)` in `dune-project` for `.mlx` files. Until the opam release, pin it:
`opam pin add melhole meltea meltea-ppx https://github.com/jongleb/meltea.git`.

## Tested by laws

Property-based tests, not scenarios:

- after any sequence of messages, the DOM is the view of `fold update init messages`;
- a message that keeps the model, or a flush with nothing pending, writes nothing;
- a hole is evaluated once on mount and again exactly when a local it reads changes;
- `Html.memo` and `Html.map` behave as in Elm;
- after any change of subscriptions, a hash change sends exactly the current ones' messages;
- TodoMVC shows the todos, the count and the route after any user actions;
- keyed loops reuse a row exactly when its item and projection are unchanged, and patching always
  shows what mounting from scratch shows, under random edits (swaps, moves, inserts, clears);
- the shared template compiler and the keyed list algorithm have their own laws, 24 of them
  checked 2,000 times each.

```console
$ dune build && dune test && dune build @runtest-js && npx vitest run
```

## Where it is developed

Meltea shares its runtime and compiler, `melhole`, with [Melact](https://github.com/jongleb/melact),
the reason-react-compatible sibling for code that already lives on React. Both are developed in
that repository; this one is a mirror of Meltea's part, exported by `scripts/publish-meltea.sh`.
Issues and ideas are welcome here.
