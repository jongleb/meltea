open Testing

let () =
  describe "Numbers read without raising" (fun () ->
      test "float_of_string_opt gives what Stdlib's gives for any string" (fun () ->
          let pieces =
            [| "0"; "1"; "7"; "9"; "."; "_"; "e"; "E"; "+"; "-"; "x"; "0x"; "p"; "P"; "a"; "f"; "F"; "n"; "nan"; "NaN";
               "inf"; "infinity"; "Infinity"; "i"; " "; "z"; "1e"; "e5"; "" |]
          in
          let same expected found =
            match (expected, found) with
            | Some expected, Some found -> expected = found || (Float.is_nan expected && Float.is_nan found)
            | None, None -> true
            | Some _, None | None, Some _ -> false
          in
          check
            (property (tapes ~shortest:0 ~longest:10) (fun values ->
                 let tape = tape values in
                 let text = String.concat "" (List.init (Array.length values) (fun _ -> pieces.(draw tape (Array.length pieces)))) in
                 expect (text, same (Stdlib.float_of_string_opt text) (Meltea_runtime.Numbers.float_of_string_opt text))
                 |. to_equal (text, true)))
            [%mel.obj { numRuns = 2000 }]))
