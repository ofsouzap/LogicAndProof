open LogicAndProof.Pasvide

(* Les formateurs pour les pas-vides *)

let rec int_pas_vide_element_formateur ppf = function
   | Feui x -> Fmt.pf ppf "%d" x
   | Cons (h, ts) -> Fmt.pf ppf "%d, %a" h int_pas_vide_element_formateur ts

let rec string_pas_vide_element_formateur ppf = function
  | Feui x -> Fmt.pf ppf "%s" x
  | Cons (h, ts) -> Fmt.pf ppf "%s, %a" h string_pas_vide_element_formateur ts

(* Les "testable"s personnalise pour les pas-vides *)

let int_pas_vide : (int pas_vide) Alcotest.testable = Alcotest.testable int_pas_vide_element_formateur (=)
let string_pas_vide : (string pas_vide) Alcotest.testable = Alcotest.testable string_pas_vide_element_formateur (=)

let test_singleton (t : 'a pas_vide Alcotest.testable) (x : 'a) () =
  let res = singleton x in
  let exp = Feui x in
  Alcotest.check t "" res exp

let test_singleton_int = test_singleton int_pas_vide
let test_singleton_string = test_singleton string_pas_vide

let suite_singleton =
  [ "Int0", `Quick, test_singleton_int 0
  ; "Int1", `Quick, test_singleton_int 4
  ; "Int2", `Quick, test_singleton_int (-5)
  ; "String0", `Quick, test_singleton_string ""
  ; "String1", `Quick, test_singleton_string "t"
  ; "String2", `Quick, test_singleton_string "d482faws"
  ]

let () =
  let open Alcotest in
  run "Pas-Vide"
  [ "Singleton", suite_singleton
  ]
