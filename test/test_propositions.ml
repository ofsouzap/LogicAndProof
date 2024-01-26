let sanity_test () =
  Alcotest.(check int) "adding" 5 (3+2)

let () =
  let open Alcotest in
  run "Propositions" [
    "sanity-test", [
      test_case "adding" `Quick sanity_test
    ]
  ]
