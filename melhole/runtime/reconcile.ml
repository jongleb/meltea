type step = int

type view = Keep of int | Move of int | Create

let create = -1
let keep i = 2 * i
let move i = (2 * i) + 1
let kept step = match step >= 0 && step land 1 = 0 with true -> Some (step / 2) | false -> None
let moved step = match step >= 0 && step land 1 = 1 with true -> Some (step / 2) | false -> None

let view step =
  match (kept step, moved step) with Some i, _ -> Keep i | _, Some i -> Move i | None, None -> Create

type t = { steps : step array; dropped : int array }

let plan ~old ~next =
  let old_length = Array.length old and next_length = Array.length next in
  let steps = Array.make next_length create in
  let same i j = String.equal (Array.unsafe_get old i) (Array.unsafe_get next j) in
  let rec narrow old_start old_stop next_start next_stop =
    match (old_start < old_stop && next_start < next_stop) with
    | false -> (old_start, old_stop, next_start, next_stop)
    | true ->
        if same old_start next_start then (
          Array.unsafe_set steps next_start (keep old_start);
          narrow (old_start + 1) old_stop (next_start + 1) next_stop)
        else if same (old_stop - 1) (next_stop - 1) then (
          Array.unsafe_set steps (next_stop - 1) (keep (old_stop - 1));
          narrow old_start (old_stop - 1) next_start (next_stop - 1))
        else if
          old_stop - old_start >= 3
          && next_stop - next_start >= 3
          && same old_start (next_stop - 1)
          && same (old_stop - 1) next_start
          && same (old_start + 1) (next_start + 1)
        then (
          Array.unsafe_set steps next_start (move (old_stop - 1));
          Array.unsafe_set steps (next_stop - 1) (move old_start);
          narrow (old_start + 1) (old_stop - 1) (next_start + 1) (next_stop - 1))
        else (old_start, old_stop, next_start, next_stop)
  in
  let old_start, old_stop, next_start, next_stop = narrow 0 old_length 0 next_length in
  let range from stop = Array.init (stop - from) (fun x -> from + x) in
  match (old_start < old_stop, next_start < next_stop) with
  | false, _ -> { steps; dropped = [||] }
  | true, false -> { steps; dropped = range old_start old_stop }
  | true, true ->
      let rec capacity size = if size >= 2 * (old_stop - old_start) then size else capacity (2 * size) in
      let size = capacity 8 in
      let table = Array.make size (-1) and claimed = Array.make old_length 0 in
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
          match (String.equal (Array.unsafe_get old i) key, Array.unsafe_get claimed i) with
          | true, 0 ->
              Array.unsafe_set claimed i 1;
              Some i
          | true, _ | false, _ -> claim key ((at + 1) land (size - 1))
      in
      let span = next_stop - next_start in
      let targets = Array.make span 0 and sources = Array.make span 0 in
      let reused = ref 0 in
      for j = next_start to next_stop - 1 do
        let key = Array.unsafe_get next j in
        Option.iter
          (fun i ->
            Array.unsafe_set steps j (move i);
            Array.unsafe_set targets !reused j;
            Array.unsafe_set sources !reused i;
            incr reused)
          (claim key (slot key))
      done;
      let count = !reused in
      let tails = Array.make count 0 and previous = Array.make count (-1) in
      let longest = ref 0 in
      for x = 0 to count - 1 do
        let source = Array.unsafe_get sources x in
        let rec search low high =
          if low >= high then low
          else
            let middle = (low + high) / 2 in
            if Array.unsafe_get sources (Array.unsafe_get tails middle) < source then search (middle + 1) high
            else search low middle
        in
        let at = search 0 !longest in
        Array.unsafe_set previous x (if at = 0 then -1 else Array.unsafe_get tails (at - 1));
        Array.unsafe_set tails at x;
        longest := max !longest (at + 1)
      done;
      let rec kept_back x =
        if x >= 0 then (
          Array.unsafe_set steps (Array.unsafe_get targets x) (keep (Array.unsafe_get sources x));
          kept_back (Array.unsafe_get previous x))
      in
      if !longest > 0 then kept_back (Array.unsafe_get tails (!longest - 1));
      let unclaimed = ref 0 in
      for i = old_start to old_stop - 1 do
        if Array.unsafe_get claimed i = 0 then incr unclaimed
      done;
      let dropped = Array.make !unclaimed 0 in
      let at = ref 0 in
      for i = old_start to old_stop - 1 do
        if Array.unsafe_get claimed i = 0 then (
          Array.unsafe_set dropped !at i;
          incr at)
      done;
      { steps; dropped }
