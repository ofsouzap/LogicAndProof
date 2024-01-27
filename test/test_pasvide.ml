open LogicAndProof.Pasvide

(* Les formateurs pour les pas-vides *)

let rec int_pas_vide_element_formateur ppf = function
   | Feui x -> Fmt.pf ppf "%d" x
   | Cons (h, ts) -> Fmt.pf ppf "%d, %a" h int_pas_vide_element_formateur ts

let int_pas_vide_opt_element_formateur ppf = function
  | None -> Fmt.pf ppf ""
  | Some (Feui x) -> Fmt.pf ppf "%d" x
  | Some (Cons (h, ts)) -> Fmt.pf ppf "%d, %a" h int_pas_vide_element_formateur ts

let rec int_paire_pas_vide_element_formateur ppf = function
  | Feui (x1, x2) -> Fmt.pf ppf "(%d, %d)" x1 x2
  | Cons ((h1, h2), ts) -> Fmt.pf ppf "(%d, %d), %a" h1 h2 int_paire_pas_vide_element_formateur ts

let rec string_pas_vide_element_formateur ppf = function
  | Feui x -> Fmt.pf ppf "%s" x
  | Cons (h, ts) -> Fmt.pf ppf "%s, %a" h string_pas_vide_element_formateur ts

(* Les "testable"s personnalise pour les pas-vides *)

let rec comparez_lists_set (xs : 'a list) (ys : 'a list) : bool =
  let rec essaiez_enlever (x : 'a) (acc : 'a list) = function
    | [] -> None
    | h::ts ->
      if h = x
      then Some (acc @ ts)
      else essaiez_enlever x (acc @ [h]) ts
  in
  match (xs, ys) with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | (xh::xts), ys -> ( match essaiez_enlever xh [] ys with
      | None -> false
      | Some ys' -> comparez_lists_set xts ys' )

let comparez_pas_vide_set (xs : 'a pas_vide) (ys : 'a pas_vide) : bool =
  comparez_lists_set (list_of_pas_vide xs) (list_of_pas_vide ys)

let int_pas_vide : (int pas_vide) Alcotest.testable = Alcotest.testable int_pas_vide_element_formateur ( = )
let int_pas_vide_opt : (int pas_vide option) Alcotest.testable = Alcotest.testable int_pas_vide_opt_element_formateur ( = )
let int_paire_pas_vide : ((int * int) pas_vide) Alcotest.testable = Alcotest.testable int_paire_pas_vide_element_formateur ( = )
let string_pas_vide : (string pas_vide) Alcotest.testable = Alcotest.testable string_pas_vide_element_formateur ( = )
let int_paire_pas_vide_set : ((int * int) pas_vide) Alcotest.testable = Alcotest.testable int_paire_pas_vide_element_formateur comparez_pas_vide_set

(* Testes Singleton *)

let test_singleton (t : 'a pas_vide Alcotest.testable) (x : 'a) () =
  let res = singleton x in
  let exp = Feui x in
  Alcotest.check t "" exp res

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

(* Testes ajoutez *)

let test_ajoutez t x1 x2 () =
  let res = ajoutez x2 (singleton x1) in
  let exp = Cons (x2, Feui x1) in
  Alcotest.check t "" exp res

let test_ajoutez_int = test_ajoutez int_pas_vide
let test_ajoutez_string = test_ajoutez string_pas_vide

let suite_ajoutez =
  [ "Int0", `Quick, test_ajoutez_int 3 1
  ; "Int1", `Quick, test_ajoutez_int (-2) 0
  ; "String0", `Quick, test_ajoutez_string "tgrw" "qed"
  ; "String1", `Quick, test_ajoutez_string "tgrs" "dqe"
  ]

(* Testes apposez *)

let test_apposez t x1 x2 () =
  let res = apposez x2 (singleton x1) in
  let exp = Cons (x1, Feui x2) in
  Alcotest.check t "" exp res

let test_apposez_int = test_apposez int_pas_vide
let test_apposez_string = test_apposez string_pas_vide

let suite_apposez =
  [ "Int0", `Quick, test_apposez_int 3 1
  ; "Int1", `Quick, test_apposez_int (-2) 0
  ; "String0", `Quick, test_apposez_string "tgrw" "qed"
  ; "String1", `Quick, test_apposez_string "tgrs" "dqe"
  ]

(* Testes tete *)

let test_tete exp xs () =
  let res = tete xs in
  Alcotest.(check int) "" exp res

let suite_tete =
  [ "Int0", `Quick, test_tete 3 (Feui 3)
  ; "Int1", `Quick, test_tete 4 (Cons (4, Feui 5))
  ; "Int2", `Quick, test_tete 0 (Cons (0, Cons (5, Feui 6)))
  ]

(* Testes tail *)

let test_tail t exp xs () =
  let res = tail xs in
  Alcotest.check t "" exp res

let test_tail_int = test_tail int_pas_vide_opt

let suite_tail =
  [ "Int0", `Quick, test_tail_int None (Feui 3)
  ; "Int1", `Quick, test_tail_int (Some (Feui 5)) (Cons (4, Feui 5))
  ; "Int2", `Quick, test_tail_int (Some (Cons (5, Feui 6))) (Cons (0, Cons (5, Feui 6)))
  ]

(* Testes renverse *)

let test_renverse t exp xs () =
  let res = renverse xs in
  Alcotest.check t "" exp res

let test_renverse_int = test_renverse int_pas_vide

let suite_renverse =
  [ "Int0", `Quick, test_renverse_int (Feui 3) (Feui 3)
  ; "Int1", `Quick, test_renverse_int (Cons (5, Feui 4)) (Cons (4, Feui 5))
  ; "Int2", `Quick, test_renverse_int (Cons (6, Cons (5, Feui 0))) (Cons (0, Cons (5, Feui 6)))
  ]

(* Testes pas_vide_of_list *)

let test_pas_vide_of_list t exp xs () =
  let res = pas_vide_of_list xs in
  Alcotest.check t "" exp res

let test_pas_vide_of_list_int = test_pas_vide_of_list int_pas_vide

let suite_pas_vide_of_list =
  [ "Int0", `Quick, test_pas_vide_of_list_int (Feui 3) [3]
  ; "Int1", `Quick, test_pas_vide_of_list_int (Cons (4, Feui 5)) [4;5]
  ; "Int2", `Quick, test_pas_vide_of_list_int (Cons (0, Cons (5, Feui 6))) [0;5;6]
  ]

(* Testes list_of_pas_vide *)

let test_list_of_pas_vide_int exp xs () =
  let res = list_of_pas_vide xs in
  Alcotest.(check (list int)) "" exp res

let suite_list_of_pas_vide =
  [ "Int0", `Quick, test_list_of_pas_vide_int [3] (Feui 3)
  ; "Int1", `Quick, test_list_of_pas_vide_int [4;5] (Cons (4, Feui 5))
  ; "Int2", `Quick, test_list_of_pas_vide_int [0;5;6] (Cons (0, Cons (5, Feui 6)))
  ]

(* Testes map/map_rev *)

let test_mapf t mapf exp f xs () =
  let res = mapf f xs in
  Alcotest.check t "" exp res

let test_map_int = test_mapf int_pas_vide map
let test_map_rev_int = test_mapf int_pas_vide map_rev

let suite_map_map_rev =
  [ "Int0", `Quick, test_map_int          (Feui 4)                          (( + ) 1)         (Feui 3)
  ; "Int0 rev", `Quick, test_map_rev_int  (Feui 4)                          (( + ) 1)         (Feui 3)
  ; "Int1", `Quick, test_map_int          (Cons (8, Feui 10))               (( * ) 2)         (Cons (4, Feui 5))
  ; "Int1 rev", `Quick, test_map_rev_int  (Cons (10, Feui 8))               (( * ) 2)         (Cons (4, Feui 5))
  ; "Int2", `Quick, test_map_int          (Cons ((-1), Cons (4, Feui 5)))   (fun x -> x - 1)  (Cons (0, Cons (5, Feui 6)))
  ; "Int2 rev", `Quick, test_map_rev_int  (Cons (5, Cons (4, Feui (-1))))   (fun x -> x - 1)  (Cons (0, Cons (5, Feui 6)))
  ]

(* Testes zip/zip_rev *)

let test_zipf t zipf exp xs ys () =
  let res = zipf xs ys in
  Alcotest.check t "" exp res

let test_zip_int = test_zipf int_paire_pas_vide zip
let test_zip_rev_int = test_zipf int_paire_pas_vide zip_rev

let suite_zip_zip_rev =
  [ "Int_eq0", `Quick, test_zip_int           (Feui (3, 4))                             (Feui 3)                      (Feui 4)
  ; "Int_eq0 rev", `Quick, test_zip_rev_int   (Feui (3, 4))                             (Feui 3)                      (Feui 4)
  ; "Int_eq1", `Quick, test_zip_int           (Cons ((3, 7), Feui (4, 3)))              (Cons (3, Feui 4))            (Cons (7, Feui 3))
  ; "Int_eq1 rev", `Quick, test_zip_rev_int   (Cons ((4, 3), Feui (3, 7)))              (Cons (3, Feui 4))            (Cons (7, Feui 3))
  ; "Int_eq2", `Quick, test_zip_int           (Cons ((7,1), Cons ((8,2), Feui (9,3))))  (Cons (7, Cons (8, Feui 9)))  (Cons (1, Cons (2, Feui 3)))
  ; "Int_eq2 rev", `Quick, test_zip_rev_int   (Cons ((9,3), Cons ((8,2), Feui (7,1))))  (Cons (7, Cons (8, Feui 9)))  (Cons (1, Cons (2, Feui 3)))
  ; "Int_l0", `Quick, test_zip_int            (Feui (7, 5))                             (Cons (7, Cons (8, Feui 9)))  (Feui 5)
  ; "Int_l0 rev", `Quick, test_zip_rev_int    (Feui (7, 5))                             (Cons (7, Cons (8, Feui 9)))  (Feui 5)
  ; "Int_r0", `Quick, test_zip_int            (Feui (5, 7))                             (Feui 5)                      (Cons (7, Cons (8, Feui 9)))
  ; "Int_r0 rev", `Quick, test_zip_rev_int    (Feui (5, 7))                             (Feui 5)                      (Cons (7, Cons (8, Feui 9)))
  ]

(* Testes tous *)

let test_tous exp f xs () =
  let res = tous f xs in
  Alcotest.(check bool) "" exp res

let suite_tous =
  [ "Int0", `Quick, test_tous true  (fun x -> x mod 2 = 0)    (Feui 2)
  ; "Int1", `Quick, test_tous false (fun x -> x mod 2 = 0)    (Feui 1)
  ; "Int2", `Quick, test_tous false (fun x -> x mod 2 = 0)    (Cons (5, Cons (2, Feui 6)))
  ; "Int3", `Quick, test_tous false (fun x -> x mod 2 = 0)    (Cons (5, Cons (1, Feui 7)))
  ; "Int4", `Quick, test_tous true  (fun x -> x mod 2 = 0)    (Cons (6, Cons (2, Feui 6)))
  ]

(* Testes quelque *)

let test_quelque exp f xs () =
  let res = quelque f xs in
  Alcotest.(check bool) "" exp res

let suite_quelque =
  [ "Int0", `Quick, test_quelque true  (fun x -> x mod 2 = 0)    (Feui 2)
  ; "Int1", `Quick, test_quelque false (fun x -> x mod 2 = 0)    (Feui 1)
  ; "Int2", `Quick, test_quelque true  (fun x -> x mod 2 = 0)    (Cons (5, Cons (2, Feui 6)))
  ; "Int3", `Quick, test_quelque false (fun x -> x mod 2 = 0)    (Cons (5, Cons (1, Feui 7)))
  ; "Int4", `Quick, test_quelque true  (fun x -> x mod 2 = 0)    (Cons (6, Cons (2, Feui 6)))
  ]

(* Testes Produit Cartesian *)

let test_prod_cartesian exp xs ys () =
  let res = prod_cartesian xs ys in
  Alcotest.check int_paire_pas_vide_set "" exp res

let suite_prod_cartesian =
  [ "0", `Quick, test_prod_cartesian
      (pas_vide_of_list [ 1,-1; 1,-2; 2,-1; 2,-2 ])
      (pas_vide_of_list [ 1; 2 ])
      (pas_vide_of_list [ -1; -2 ])
  ; "1", `Quick, test_prod_cartesian
      (pas_vide_of_list [ 1,-1; 1,-2 ])
      (pas_vide_of_list [ 1 ])
      (pas_vide_of_list [ -1; -2 ])
  ; "2", `Quick, test_prod_cartesian
      (pas_vide_of_list [ 1,-1; 2,-1 ])
      (pas_vide_of_list [ 1; 2 ])
      (pas_vide_of_list [ -1 ])
  ; "3", `Quick, test_prod_cartesian
      (pas_vide_of_list [ 1,-1 ])
      (pas_vide_of_list [ 1 ])
      (pas_vide_of_list [ -1 ])
  ; "4", `Quick, test_prod_cartesian
      (pas_vide_of_list [ 0,0; 0,1; 0,2; 1,0; 1,1; 1,2 ])
      (pas_vide_of_list [ 0; 1 ])
      (pas_vide_of_list [ 0; 1; 2 ])
  ]

(* Main *)

let () =
  let open Alcotest in
  run "Pas-Vide"
  [ "Singleton", suite_singleton
  ; "Ajoutez", suite_ajoutez
  ; "Apposez", suite_apposez
  ; "Tete", suite_tete
  ; "Tail", suite_tail
  ; "Renverse", suite_renverse
  ; "Pas-Vide of List", suite_pas_vide_of_list
  ; "List of Pas-Vide", suite_list_of_pas_vide
  ; "Map/Map-Rev", suite_map_map_rev
  ; "Zip/Zip-Rev", suite_zip_zip_rev
  ; "Tous", suite_tous
  ; "Quelque", suite_quelque
  ; "Produit Cartesian", suite_prod_cartesian
  ]
