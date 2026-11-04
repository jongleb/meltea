type step = Keep of int | Move of int | Create
type t = { steps : step array; dropped : int list }

let plan ~old ~next =
  let old_length = Array.length old and next_length = Array.length next in
  let steps = Array.make next_length Create in
  let rec prefix i =
    if i < old_length && i < next_length && String.equal old.(i) next.(i) then (
      steps.(i) <- Keep i;
      prefix (i + 1))
    else i
  in
  let start = prefix 0 in
  let rec suffix old_last next_last =
    if
      old_last >= start && next_last >= start
      && String.equal old.(old_last) next.(next_last)
    then (
      steps.(next_last) <- Keep old_last;
      suffix (old_last - 1) (next_last - 1))
    else (old_last + 1, next_last + 1)
  in
  let old_stop, next_stop = suffix (old_length - 1) (next_length - 1) in
  let unclaimed = Array.init (max 0 (old_stop - start)) (fun x -> start + x) in
  Array.stable_sort (fun x y -> String.compare old.(x) old.(y)) unclaimed;
  let claims = Array.make (Array.length unclaimed) `Unclaimed in
  let rec first key low high =
    if low >= high then low
    else
      let middle = (low + high) / 2 in
      if String.compare old.(unclaimed.(middle)) key < 0 then first key (middle + 1) high
      else first key low middle
  in
  let rec claim key at =
    if at < Array.length unclaimed && String.equal old.(unclaimed.(at)) key then
      match claims.(at) with
      | `Unclaimed ->
          claims.(at) <- `Claimed;
          Some unclaimed.(at)
      | `Claimed -> claim key (at + 1)
    else None
  in
  let reused = ref [] in
  for j = next_stop - 1 downto start do
    Option.iter
      (fun i ->
        steps.(j) <- Move i;
        reused := (j, i) :: !reused)
      (claim next.(j) (first next.(j) 0 (Array.length unclaimed)))
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
          if snd reused.(tails.(middle)) < source then search (middle + 1) high
          else search low middle
      in
      let at = search 0 !longest in
      previous.(x) <- (if at = 0 then None else Some tails.(at - 1));
      tails.(at) <- x;
      longest := max !longest (at + 1))
    reused;
  let rec keep x =
    let j, i = reused.(x) in
    steps.(j) <- Keep i;
    Option.iter keep previous.(x)
  in
  if !longest > 0 then keep tails.(!longest - 1);
  let dropped = ref [] in
  Array.iteri
    (fun at claim ->
      match claim with `Unclaimed -> dropped := unclaimed.(at) :: !dropped | `Claimed -> ())
    claims;
  { steps; dropped = !dropped }
