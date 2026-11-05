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
      let unclaimed = Array.init (max 0 (old_stop - old_start)) (fun x -> old_start + x) in
      Array.stable_sort (fun x y -> String.compare (Array.unsafe_get old x) (Array.unsafe_get old y)) unclaimed;
      let claims = Array.make (Array.length unclaimed) `Unclaimed in
      let rec first key low high =
        if low >= high then low
        else
          let middle = (low + high) / 2 in
          if String.compare (Array.unsafe_get old (Array.unsafe_get unclaimed middle)) key < 0 then first key (middle + 1) high
          else first key low middle
      in
      let rec claim key at =
        if at < Array.length unclaimed && String.equal (Array.unsafe_get old (Array.unsafe_get unclaimed at)) key then
          match Array.unsafe_get claims at with
          | `Unclaimed ->
              Array.unsafe_set claims at `Claimed;
              Some (Array.unsafe_get unclaimed at)
          | `Claimed -> claim key (at + 1)
        else None
      in
      let reused = ref [] in
      for j = next_stop - 1 downto next_start do
        Option.iter
          (fun i ->
            Array.unsafe_set steps j (Move i);
            reused := (j, i) :: !reused)
          (claim (Array.unsafe_get next j) (first (Array.unsafe_get next j) 0 (Array.length unclaimed)))
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
      let dropped = ref [] in
      Array.iteri
        (fun at claim ->
          match claim with `Unclaimed -> dropped := (Array.unsafe_get unclaimed at) :: !dropped | `Claimed -> ())
        claims;
      { steps; dropped = !dropped }
