open LogicAndProof.Sets

let int_minu = QCheck.int_bound 20

let test_vide_int =
  "Vide", `Quick, ( fun () -> Alcotest.(check (list int)) "" [] (list_of_set vide) )

let test_singleton =
  QCheck.Test.make ~count:1000 ~name:"Singleton"
  QCheck.int
  ( fun x -> list_of_set (singleton x) = [x])

let suite_contruction =
  test_vide_int
  :: List.map QCheck_alcotest.to_alcotest
  [ test_singleton ]

let test_ajoutez =
  QCheck.Test.make ~count:100 ~name:"Ajoutez"
  QCheck.(pair (set_arbitraire_max 10 int) int)
  ( fun (xs, x) ->
    membre x (ajoutez x xs) )

let suite_unaire = List.map QCheck_alcotest.to_alcotest
  [ test_ajoutez ]

let test_intersection_peut_etre_dedans =
  QCheck.Test.make ~count:1000 ~name:"Intersection"
  QCheck.(triple (set_arbitraire_max 10 int_minu) (set_arbitraire_max 10 int_minu) int_minu)
  ( fun (xs,ys,z) -> let zs = intersection xs ys in
    membre z zs = (membre z xs && membre z ys) )

let test_intersection_dedans_un_ou_deux =
  QCheck.Test.make ~count:1000 ~name:"Intersection dedans"
  QCheck.(quad (set_arbitraire_max 10 int_minu) (set_arbitraire_max 10 int_minu) int_minu bool)
  ( fun (xs',ys',z,b) ->
    let xs = if b then ajoutez z xs' else xs' in
    let ys = if b then ys' else ajoutez z ys' in
    let zs = intersection xs ys in
    membre z zs = (membre z xs && membre z ys) )

let test_intersection_dedans_les_deux =
  QCheck.Test.make ~count:1000 ~name:"Intersection dans les deux"
  QCheck.(triple (set_arbitraire_max 10 int_minu) (set_arbitraire_max 10 int_minu) int_minu)
  ( fun (xs',ys',z) ->
    let xs = ajoutez z xs' in
    let ys = ajoutez z ys' in
    let zs = intersection xs ys in
    membre z zs )

let test_union_peut_etre_dedans =
  QCheck.Test.make ~count:1000 ~name:"Union"
  QCheck.(triple (set_arbitraire_max 10 int_minu) (set_arbitraire_max 10 int_minu) int_minu)
  ( fun (xs,ys,z) -> let zs = union xs ys in
    membre z zs = (membre z xs || membre z ys) )

let test_union_dedans_un_ou_deux =
  QCheck.Test.make ~count:1000 ~name:"Union dedans"
  QCheck.(quad (set_arbitraire_max 10 int_minu) (set_arbitraire_max 10 int_minu) int_minu bool)
  ( fun (xs',ys',z,b) ->
    let xs = if b then ajoutez z xs' else xs' in
    let ys = if b then ys' else ajoutez z ys' in
    let zs = union xs ys in
    membre z zs )

let test_union_dedans_les_deux =
  QCheck.Test.make ~count:1000 ~name:"Union dans les deux"
  QCheck.(triple (set_arbitraire_max 10 int_minu) (set_arbitraire_max 10 int_minu) int_minu)
  ( fun (xs',ys',z) ->
    let xs = ajoutez z xs' in
    let ys = ajoutez z ys' in
    let zs = union xs ys in
    membre z zs )

let test_pareil_quand_pareil =
  QCheck.Test.make ~count:0 ~name:"Pareil quand pareil"
  (QCheck.make @@ QCheck.Gen.(list int >>= ( fun xs -> pair (return xs) (shuffle_l xs)) ))
  ( fun (xs',ys') ->
    let xs = set_of_list xs' in
    let ys = set_of_list ys' in
    pareil xs ys )

let test_pareil_pas_pareil =
  QCheck.Test.make ~count:0 ~name:"Pareil quand pas pareil"
  QCheck.(triple (list int) (list int) int)
  ( fun (xs'',ys',z) -> if List.mem z ys'
    then true
    else
      let xs' = z::xs'' in
      let xs = set_of_list xs' in
      let ys = set_of_list ys' in
      not (pareil xs ys) )

let suite_binaire = List.map QCheck_alcotest.to_alcotest
  [ test_intersection_peut_etre_dedans
  ; test_intersection_dedans_un_ou_deux
  ; test_intersection_dedans_les_deux
  ; test_union_peut_etre_dedans
  ; test_union_dedans_un_ou_deux
  ; test_union_dedans_les_deux
  ; test_pareil_quand_pareil
  ; test_pareil_pas_pareil ]

let () =
  let open Alcotest in
  run "Sets"
  [ "Construction", suite_contruction
  ; "Unaire", suite_unaire
  ; "Operations binaires", suite_binaire ]
