type kind = {
  arranged :
    'item. ('item array -> 'item array -> Reconcile.key array -> ('item -> Reconcile.key[@u]) -> View.arrangement option[@u]);
}

(** The collection a fact started from, whatever its type. *)
type earlier

type weak

(** What is known about a value: it is [before] changed as [kind] says. *)
type fact = { before : weak; kind : kind }

type facts

external weak : 'value -> weak = "WeakRef" [@@mel.new]
external deref : weak -> earlier Js.undefined = "deref" [@@mel.send]
external facts : unit -> facts = "WeakMap" [@@mel.new]
external known : facts -> 'after -> fact Js.undefined = "get" [@@mel.send]
external remember : facts -> 'after -> fact -> unit = "set" [@@mel.send]

let table = facts ()

let appended =
  {
    arranged =
      (fun [@u] items old _ _ ->
        match Array.length items > Array.length old with
        | true -> Some (`Inserted (Array.length old, Array.length items))
        | false -> None);
  }

let cut from until =
  {
    arranged =
      (fun [@u] items old _ _ ->
        match 0 <= from && from < until && Array.length old - Array.length items = until - from with
        | true -> Some (`Removed (from, until))
        | false -> None);
  }

let exchanged first second =
  let low = min first second and high = max first second in
  {
    arranged =
      (fun [@u] items old keys _ ->
        match Array.length items = Array.length old && 0 <= low && low < high && high < Array.length items with
        | true -> Some (`Shuffled ([| (low, Array.unsafe_get keys high); (high, Array.unsafe_get keys low) |], [| 1; 0 |], []))
        | false -> None);
  }

let rewritten places =
  {
    arranged =
      (fun [@u] items old keys key ->
        Array.fold_left
          (fun found place ->
            Option.bind found (fun renewed ->
                match
                  0 <= place
                  && place < Array.length items
                  && Dom_api.same (key (Array.unsafe_get items place) [@u]) (Array.unsafe_get keys place)
                with
                | true -> Some (place :: renewed)
                | false -> None))
          (match Array.length items = Array.length old with true -> Some [] | false -> None)
          places
        |> Option.map (fun renewed -> `In_place (List.rev renewed)));
  }

let strided step from =
  {
    arranged =
      (fun [@u] items old keys key ->
        let rec renewed place found =
          match place < Array.length items with
          | false -> Some (`In_place (List.rev found))
          | true -> (
              match Dom_api.same (key (Array.unsafe_get items place) [@u]) (Array.unsafe_get keys place) with
              | true -> renewed (place + step) (place :: found)
              | false -> None)
        in
        match Array.length items = Array.length old with true -> renewed from [] | false -> None);
  }

let record kind before after =
  remember table after { before = weak before; kind };
  after

let () =
  View.hinting :=
    {
      hint =
        (fun [@u] (Input before) (Input after) items old keys key ->
          Option.bind (Js.Undefined.toOption (known table after)) (fun fact ->
              match Dom_api.same (deref fact.before) before with
              | true -> fact.kind.arranged items old keys key [@u]
              | false -> None));
    }
