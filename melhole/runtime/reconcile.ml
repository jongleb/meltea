type step = Keep of int | Move of int | Create
type t = { steps : step array; dropped : int list }

let plan ~old ~next =
  let old_length = Array.length old and next_length = Array.length next in
  let steps = Array.make next_length Create in
  let same i j = String.equal (Array.unsafe_get old i) (Array.unsafe_get next j) in
  let rec narrow old_start old_stop next_start next_stop =
    match (old_start < old_stop && next_start < next_stop) with
    | false -> (old_start, old_stop, next_start, next_stop)
    | true ->
        if same old_start next_start then (
          Array.unsafe_set steps next_start (Keep old_start);
          narrow (old_start + 1) old_stop (next_start + 1) next_stop)
        else if same (old_stop - 1) (next_stop - 1) then (
          Array.unsafe_set steps (next_stop - 1) (Keep (old_stop - 1));
          narrow old_start (old_stop - 1) next_start (next_stop - 1))
        else if
          old_stop - old_start >= 3
          && next_stop - next_start >= 3
          && same old_start (next_stop - 1)
          && same (old_stop - 1) next_start
          && same (old_start + 1) (next_start + 1)
        then (
          Array.unsafe_set steps next_start (Move (old_stop - 1));
          Array.unsafe_set steps (next_stop - 1) (Move old_start);
          narrow (old_start + 1) (old_stop - 1) (next_start + 1) (next_stop - 1))
        else (old_start, old_stop, next_start, next_stop)
  in
  let old_start, old_stop, next_start, next_stop = narrow 0 old_length 0 next_length in
  match (old_start < old_stop, next_start < next_stop) with
  | false, _ -> { steps; dropped = [] }
  | true, false -> { steps; dropped = List.init (old_stop - old_start) (fun x -> old_start + x) }
  | true, true ->
      let rec capacity size = if size >= 2 * (old_stop - old_start) then size else capacity (2 * size) in
      let size = capacity 8 in
      let table = Array.make size (-1) and claims = Array.make old_length `Unclaimed in
      let slot key =
        let code = ref 0 in
        String.iter (fun letter -> code := ((!code * 31) + Char.code letter) land 0x3FFFFFFF) key;
        !code land (size - 1)
      in
      for i = old_start to old_stop - 1 do
        let rec place at =
          if Array.unsafe_get table at < 0 then Array.unsafe_set table at i else place ((at + 1) land (size - 1))
        in
        place (slot (Array.unsafe_get old i))
      done;
      let rec claim key at =
        let i = Array.unsafe_get table at in
        if i < 0 then None
        else
          match (String.equal (Array.unsafe_get old i) key, Array.unsafe_get claims i) with
          | true, `Unclaimed ->
              Array.unsafe_set claims i `Claimed;
              Some i
          | true, `Claimed | false, _ -> claim key ((at + 1) land (size - 1))
      in
      let reused = ref [] in
      for j = next_stop - 1 downto next_start do
        let key = Array.unsafe_get next j in
        Option.iter
          (fun i ->
            Array.unsafe_set steps j (Move i);
            reused := (j, i) :: !reused)
          (claim key (slot key))
      done;
      let reused = Array.of_list !reused in
      let tails = Array.make (Array.length reused) 0 in
      let previous = Array.make (Array.length reused) None in
      let longest = ref 0 in
      Array.iteri
        (fun x (_, source) ->
          let rec search low high =
            if low >= high then low
            else
              let middle = (low + high) / 2 in
              if snd (Array.unsafe_get reused (Array.unsafe_get tails middle)) < source then search (middle + 1) high
              else search low middle
          in
          let at = search 0 !longest in
          Array.unsafe_set previous x (if at = 0 then None else Some (Array.unsafe_get tails (at - 1)));
          Array.unsafe_set tails at x;
          longest := max !longest (at + 1))
        reused;
      let rec keep x =
        let j, i = Array.unsafe_get reused x in
        Array.unsafe_set steps j (Keep i);
        Option.iter keep (Array.unsafe_get previous x)
      in
      if !longest > 0 then keep (Array.unsafe_get tails (!longest - 1));
      { steps; dropped = List.filter
            (fun i -> match Array.unsafe_get claims i with `Unclaimed -> true | `Claimed -> false)
            (List.init (old_stop - old_start) (fun x -> old_start + x)) }
