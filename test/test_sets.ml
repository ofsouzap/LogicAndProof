open LogicAndProof.Sets

(* Les formateurs pour les sets *)

let int_set : ('a set) Alcotest.testable = Alcotest.testable formateur_int_set pareil

let suite =
  [ "Singleton", `Quick, (fun () -> Alcotest.(check (list int)) "" [5] (list_of_set (singleton 5)))
  ; "Vide", `Quick, (fun () -> Alcotest.(check (list int)) "" [] (list_of_set vide))
  ; "Pas d'ordre", `Quick, (fun () -> Alcotest.check int_set "" (set_of_list [1;3;5]) (set_of_list [5;3;3;3;3;5;1]))
  ; "Union", `Quick, (fun () -> Alcotest.check int_set "" (set_of_list [0;2;5]) (union (set_of_list [0;2]) (set_of_list [0;5])))
  ; "Intersection", `Quick, (fun () -> Alcotest.check int_set "" (set_of_list [0;2]) (intersection (set_of_list [0;1;2;3;4]) (set_of_list [-1;-2;0;5;2;2])))
  (* TODO - plus testes *)
  ]

let () =
  let open Alcotest in
  run "Sets"
  [ "", suite ]
