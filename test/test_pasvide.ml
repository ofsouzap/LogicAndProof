open LogicAndProof.Pasvide

let rec mon_zip xs ys = match (xs,ys) with
  | [],_ -> []
  | _,[] -> []
  | xh::xts,yh::yts -> (xh,yh) :: mon_zip xts yts

(* Les formateurs pour les pas-vides *)

let rec int_paire_pas_vide_element_formateur ppf = function
  | Feui (x1, x2) -> Fmt.pf ppf "(%d, %d)" x1 x2
  | Cons ((h1, h2), ts) -> Fmt.pf ppf "(%d, %d), %a" h1 h2 int_paire_pas_vide_element_formateur ts

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

let int_paire_pas_vide_set : ((int * int) pas_vide) Alcotest.testable = Alcotest.testable int_paire_pas_vide_element_formateur comparez_pas_vide_set

(* Testes *)

let test_singleton =
  QCheck.Test.make ~count:1000 ~name:"Singleton"
  QCheck.int
  ( fun x -> Feui x = singleton x )

let test_ajoutez =
  QCheck.Test.make ~count:1000 ~name:"Ajoutez"
  QCheck.(pair int (pas_vide_arbitraire int))
  ( fun (h,ts) ->
    let f = function | Feui _ -> false | Cons (x,_) -> x = h in
    f (ajoutez h ts) )

let test_apposez =
  QCheck.Test.make ~count:1000 ~name:"Apposez"
  QCheck.(pair int (pas_vide_arbitraire int))
  ( fun (x,ts) ->
    let rec f = function | Feui a -> a = x | Cons (_,bs) -> f bs in
    f (apposez x ts) )

let test_enchainez =
  QCheck.Test.make ~count:1000 ~name:"Enchainez"
  QCheck.(pair (pas_vide_arbitraire int) (pas_vide_arbitraire int))
  ( fun (x1,x2) ->
    list_of_pas_vide (enchainez x1 x2) = list_of_pas_vide x1 @ list_of_pas_vide x2 )

let suite_constructions = List.map QCheck_alcotest.to_alcotest
  [ test_singleton
  ; test_ajoutez
  ; test_apposez
  ; test_enchainez ]

let test_tete =
  QCheck.Test.make ~count:1000 ~name:"Tete"
  QCheck.(pair int (pas_vide_arbitraire int))
  ( fun (h,ts) ->
    let xs = ajoutez h ts in
    tete xs = h )

let test_tail =
  QCheck.Test.make ~count:1000 ~name:"Tail"
  QCheck.(pas_vide_arbitraire int)
  ( fun xs ->
    match xs with
    | Feui _ -> tail xs = None
    | Cons (_,ts) -> tail xs = Some ts )

let suite_vue = List.map QCheck_alcotest.to_alcotest
  [ test_tete
  ; test_tail ]

let test_renverse =
  QCheck.Test.make ~count:1000 ~name:"Renverse"
  QCheck.(pas_vide_arbitraire int)
  ( fun xs ->
    list_of_pas_vide (renverse xs) = List.rev (list_of_pas_vide xs) )

let test_foldl =
  QCheck.Test.make ~count:1000 ~name:"Fold L"
  QCheck.(triple (fun2 Observable.int Observable.int int) (pas_vide_arbitraire int) int)
  ( fun (f',xs,z) ->
    let f = QCheck.Fn.apply f' in
    List.fold_left f z (list_of_pas_vide xs) = foldl f z xs )

let test_foldr =
  QCheck.Test.make ~count:1000 ~name:"Fold R"
  QCheck.(triple (fun2 Observable.int Observable.int int) (pas_vide_arbitraire int) int)
  ( fun (f',xs,z) ->
    let f = QCheck.Fn.apply f' in
    List.fold_right f (list_of_pas_vide xs) z = foldr f z xs )

let test_map =
  QCheck.Test.make ~count:1000 ~name:"Map"
  QCheck.(pair (fun1 Observable.int int) (pas_vide_arbitraire int))
  ( fun (f',xs) ->
    let f = QCheck.Fn.apply f' in
    List.map f (list_of_pas_vide xs) = list_of_pas_vide (map f xs) )

let test_map_rev =
  QCheck.Test.make ~count:1000 ~name:"Map Rev"
  QCheck.(pair (fun1 Observable.int int) (pas_vide_arbitraire int))
  ( fun (f',xs) ->
    let f = QCheck.Fn.apply f' in
    List.rev (List.map f (list_of_pas_vide xs)) = list_of_pas_vide (map_rev f xs) )

let test_zip =
  QCheck.Test.make ~count:1000 ~name:"Zip"
  QCheck.(pair (pas_vide_arbitraire int) (pas_vide_arbitraire int))
  ( fun (xs,ys) ->
    mon_zip (list_of_pas_vide xs) (list_of_pas_vide ys) = list_of_pas_vide (zip xs ys) )

let test_zip_rev =
  QCheck.Test.make ~count:1000 ~name:"Zip Rev"
  QCheck.(pair (pas_vide_arbitraire int) (pas_vide_arbitraire int))
  ( fun (xs,ys) ->
    List.rev (mon_zip (list_of_pas_vide xs) (list_of_pas_vide ys)) = list_of_pas_vide (zip_rev xs ys) )

let test_tous =
  QCheck.Test.make ~count:1000 ~name:"Tous"
  QCheck.(pair (fun1 Observable.int bool) (pas_vide_arbitraire int))
  ( fun (f',xs) ->
    let f = QCheck.Fn.apply f' in
    List.for_all f (list_of_pas_vide xs) = tous f xs )

let test_quelque =
  QCheck.Test.make ~count:1000 ~name:"Quelque"
  QCheck.(pair (fun1 Observable.int bool) (pas_vide_arbitraire int))
  ( fun (f',xs) ->
    let f = QCheck.Fn.apply f' in
    List.exists f (list_of_pas_vide xs) = quelque f xs )

let suite_operations_simples = List.map QCheck_alcotest.to_alcotest
  [ test_renverse
  ; test_foldl
  ; test_foldr
  ; test_map
  ; test_map_rev
  ; test_zip
  ; test_zip_rev
  ; test_tous
  ; test_quelque ]

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
  [ "Construction", suite_constructions
  ; "Vue", suite_vue
  ; "Operations Simples", suite_operations_simples
  ; "Produit Cartesian", suite_prod_cartesian ]
