type t = Hook of { base : string; dependencies : string } | Plain

let of_name name =
  let length = String.length name in
  match length >= 3 && String.equal (String.sub name 0 3) "use" && (length = 3 || Char.uppercase_ascii name.[3] = name.[3]) with
  | false -> Plain
  | true ->
      let rec trimmed length =
        match length > 0 && name.[length - 1] >= '0' && name.[length - 1] <= '9' with
        | true -> trimmed (length - 1)
        | false -> length
      in
      let kept = trimmed length in
      Hook { base = String.sub name 0 kept; dependencies = String.sub name kept (length - kept) }
