<div align="center">

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
- **Events.** `onClick=Increment` sends a message. `onInput` and `onKeyDown` take a function of
  the value or the key, `onCheck` of the checked state. Handlers are delegated to `document`.
- **Effects.** `Cmd` and `Sub` are values that describe effects; `Program` runs them.

The counter's view compiles to this JavaScript (names shortened):

```js
const template = View.template(Template.html("<div class=\"counter\"><button>-</button>…"),
  (scope, div) => {
    const minus = div.firstChild, output = minus.nextSibling, plus = output.nextSibling, …;
    return [handler_hole(scope, minus, "click"), text_hole({ parent: output, before: null }), …];
  });

const view = (count, dispatch) =>
  View.compiled(template, 1, (scope, holes) => {
    const changed = check(scope, 0, count);              // one === against the last count
    set_handler(holes[0], () => dispatch(Decrement));    // handlers are swapped, not compared
    if (changed === Changed) set_text(holes[1], String(count));
    …
  });
```

## API

| module | what it gives |
|---|---|
| `Program` | `sandbox ~init ~update ~view root`, `element ~init ~update ~view ~subscriptions root`, `dispatch`, `flush`, `stop` |
| `Html` | `text`, `int`, `float`, `none`, `list`, `keyed`, `map`, `memo` (Elm's `lazy`) |
| `Cmd` | `none`, `batch`, `map`, `message`, `perform` (a promise) |
| `Sub` | `none`, `batch`, `map`, `every` |

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
- the shared template compiler and the keyed list algorithm have their own laws, 16 of them
  checked 2,000 times each.

```console
$ dune build && dune test && dune build @runtest-js && npx vitest run
```

## Where it is developed

Meltea shares its runtime and compiler, `melhole`, with [Melact](https://github.com/jongleb/melact),
the reason-react-compatible sibling for code that already lives on React. Both are developed in
that repository; this one is a mirror of Meltea's part, exported by `scripts/publish-meltea.sh`.
Issues and ideas are welcome here.
