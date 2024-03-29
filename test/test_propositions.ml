open Nicelib
open Nicelib.Utils
open LogicAndProof.Pasvide
open LogicAndProof.Propositions
open LogicAndProof.Propositions_abr

let formateur_string_set ppf =
  let rec aux ppf = function
    | [] -> Fmt.pf ppf ""
    | h::ts -> Fmt.pf ppf "%s, %a" h aux ts
  in
  aux ppf -.- Sets.list_of_set

let varnom_set : ('a Sets.t) Alcotest.testable = Alcotest.testable formateur_string_set Sets.equal

let test_sante () =
  let exp = 5 in
  let res = 3 + 2 in
  Alcotest.(check int) "l'addition correcte" res exp

let suite_sante =
  [ "l'addition", `Quick, test_sante
  ]

(* Propositions variables libres *)

let testez_prop_var_libres (exp : varnom Sets.t) (p : proposition) () =
  let res = prop_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_prop_var_libres =
  [ "Sets.empty", `Quick, testez_prop_var_libres Sets.empty (Impl ((lit_prop vrai), (ou_prop [lit_prop faux; lit_prop vrai])))
  ; "Un", `Quick, testez_prop_var_libres (Sets.set_of_list ["A"]) (Impl ((var_prop "A"), (ou_prop [lit_prop faux; lit_prop vrai])))
  ; "Deux", `Quick, testez_prop_var_libres (Sets.set_of_list ["A";"B"]) (Impl ((var_prop "A"), (ou_prop [var_prop "B"; var_prop "B"])))
  ]

(* Evaluation *)

let testez_evaluation_prop (exp : verite) (don_i : interpretation) (don_p : proposition) () =
  let res = evaluez don_i don_p in
  Alcotest.(check bool) "evaluation" exp res

let appliquez_don_prop (nom : string) (p : proposition) =
  List.map (fun (f,s) -> nom ^ " - " ^ s, `Quick, f p)

(* (P . Q) -> P *)
let suite_evaluation_0 = appliquez_don_prop "(P.Q)->P" (Impl (Et (pas_vide_of_list [((Atome (Var "P")) : proposition); (Atome (Var "Q"))]), Atome (Var "P")))
  [ testez_evaluation_prop vrai ["P", faux; "Q", faux], "00"
  ; testez_evaluation_prop vrai ["P", faux; "Q", vrai], "01"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", faux], "10"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", vrai], "11"
  ]

(* (P + Q) -> P *)
let suite_evaluation_1 = appliquez_don_prop "(P+Q)->P" (Impl (Ou (pas_vide_of_list [(Atome (Var "P") : proposition); Atome (Var "Q")]), Atome (Var "P")))
  [ testez_evaluation_prop vrai ["P", faux; "Q", faux], "00"
  ; testez_evaluation_prop faux ["P", faux; "Q", vrai], "01"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", faux], "10"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", vrai], "11"
  ]

(* P <-> (t . (Q + R)) *)
let suite_evaluation_2 = appliquez_don_prop "P<->(t.(Q+R))" (BiImpl ((Atome (Var "P")), (Et (paire (Atome (Lit vrai) : proposition) (Ou (paire (Atome (Var "Q") : proposition) (Atome (Var "R"))))))))
  [ testez_evaluation_prop vrai ["P", faux; "Q", faux; "R", faux], "000"
  ; testez_evaluation_prop faux ["P", faux; "Q", faux; "R", vrai], "001"
  ; testez_evaluation_prop faux ["P", faux; "Q", vrai; "R", faux], "010"
  ; testez_evaluation_prop faux ["P", faux; "Q", vrai; "R", vrai], "011"
  ; testez_evaluation_prop faux ["P", vrai; "Q", faux; "R", faux], "100"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", faux; "R", vrai], "101"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", vrai; "R", faux], "110"
  ; testez_evaluation_prop vrai ["P", vrai; "Q", vrai; "R", vrai], "111"
  ]

let suite_evaluation =
    suite_evaluation_0
  @ suite_evaluation_1
  @ suite_evaluation_2

(* Comparez propositions *)

let testez_propositions_equivalents (exp : bool) (p1 : proposition) (p2 : proposition) () =
  let vars = Sets.union (prop_var_libres p1) (prop_var_libres p2) in
  let res = propositions_equivalents vars evaluez evaluez p1 p2 in
  Alcotest.(check bool) "" exp res

let suite_propositions_equivalents =
  [ "", `Quick, testez_propositions_equivalents vrai (Impl ((et_prop [var_prop "P"; var_prop "Q"]), (var_prop "P"))) (lit_prop vrai)
  ; "", `Quick, testez_propositions_equivalents vrai (et_prop [Impl ((var_prop "P"), (var_prop "Q")); Impl ((var_prop "Q"), (var_prop "P"))]) (BiImpl ((var_prop "P"), (var_prop "Q")))
  ]

(* Proposition Brute au NNF *)

let testez_prop_au_nnf =
  QCheck.Test.make ~count:1000
  proposition_arbitraire
  ( fun (prop : proposition) ->
    let simple = prop_au_simple prop in
    let nnf = simple_au_nnf simple in
    let vars = prop_var_libres prop in
    propositions_equivalents vars evaluez evaluez_nnf prop nnf )

let suite_prop_au_nnf = List.map QCheck_alcotest.to_alcotest
  [ testez_prop_au_nnf
  ]

(* NNF variables libres *)

let testez_nnf_var_libres (exp : varnom Sets.t) (p : proposition_nnf) () =
  let res = nnf_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_nnf_var_libres =
  [ "Sets.empty", `Quick, testez_nnf_var_libres Sets.empty (ou_nnf [lit_nnf vrai; ou_nnf [lit_nnf faux; lit_nnf vrai]])
  ; "Un", `Quick, testez_nnf_var_libres (Sets.set_of_list ["A"]) (ou_nnf [lit_nnf vrai; ou_nnf [pas_var_nnf "A"; lit_nnf vrai]])
  ; "Deux", `Quick, testez_nnf_var_libres (Sets.set_of_list ["A";"B"]) (ou_nnf [var_nnf "A"; ou_nnf [pas_var_nnf "B"; var_nnf "B"]])
  ]

(* Evaluez NNF *)

let testez_evaluation_nnf (exp : verite) (don_i : interpretation) (don_p : proposition_nnf) () =
  let res = evaluez_nnf don_i don_p in
  Alcotest.(check bool) "evaluation" exp res

let appliquez_don_nnf (nom : string) (p : proposition_nnf) =
  List.map (fun (f,s) -> nom ^ " - " ^ s, `Quick, f p)

(* A . ¬ B *)
let suite_evaluation_nnf_0 = appliquez_don_nnf "A.¬B" (et_nnf [var_nnf "A"; pas_var_nnf "B"])
  [ testez_evaluation_nnf faux ["A", faux; "B", faux], "00"
  ; testez_evaluation_nnf faux ["A", faux; "B", vrai], "01"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", faux], "10"
  ; testez_evaluation_nnf faux ["A", vrai; "B", vrai], "11"
  ]

(* A + B + (C . ¬D) *)
let suite_evaluation_nnf_1 = appliquez_don_nnf "A+B+(C+¬D)" (ou_nnf [var_nnf "A"; var_nnf "B"; et_nnf [var_nnf "C"; pas_var_nnf "D"] ])
  [ testez_evaluation_nnf faux ["A", faux; "B", faux; "C", faux; "D", faux], "0000"
  ; testez_evaluation_nnf faux ["A", faux; "B", faux; "C", faux; "D", vrai], "0001"
  ; testez_evaluation_nnf vrai ["A", faux; "B", faux; "C", vrai; "D", faux], "0010"
  ; testez_evaluation_nnf faux ["A", faux; "B", faux; "C", vrai; "D", vrai], "0011"
  ; testez_evaluation_nnf vrai ["A", faux; "B", vrai; "C", faux; "D", faux], "0100"
  ; testez_evaluation_nnf vrai ["A", faux; "B", vrai; "C", faux; "D", vrai], "0101"
  ; testez_evaluation_nnf vrai ["A", faux; "B", vrai; "C", vrai; "D", faux], "0110"
  ; testez_evaluation_nnf vrai ["A", faux; "B", vrai; "C", vrai; "D", vrai], "0111"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", faux; "C", faux; "D", faux], "1000"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", faux; "C", faux; "D", vrai], "1001"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", faux; "C", vrai; "D", faux], "1010"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", faux; "C", vrai; "D", vrai], "1011"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", vrai; "C", faux; "D", faux], "1100"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", vrai; "C", faux; "D", vrai], "1101"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", vrai; "C", vrai; "D", faux], "1110"
  ; testez_evaluation_nnf vrai ["A", vrai; "B", vrai; "C", vrai; "D", vrai], "1111"
  ]

let suite_evaluation_nnf =
    suite_evaluation_nnf_0
  @ suite_evaluation_nnf_1

(* NNF au DNF *)

let testez_nnf_au_dnf =
  QCheck.Test.make ~count:1000
  nnf_arbitraire
  ( fun (nnf : proposition_nnf) ->
    let dnf = nnf_au_dnf nnf in
    let vars = nnf_var_libres nnf in
    propositions_equivalents vars evaluez_nnf evaluez_dnf nnf dnf )

let suite_nnf_au_dnf = List.map QCheck_alcotest.to_alcotest
  [ testez_nnf_au_dnf ]

(* DNF variables libres *)

let testez_dnf_var_libres (exp : varnom Sets.t) (p : proposition_dnf) () =
  let res = dnf_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_dnf_var_libres =
  [ "Sets.empty", `Quick, testez_dnf_var_libres Sets.empty (dnf [[lit_neg_atome vrai]; [lit_neg_atome faux; lit_neg_atome vrai]])
  ; "Un", `Quick, testez_dnf_var_libres (Sets.set_of_list ["A"]) (dnf [[var_neg_atome "A"]; [lit_neg_atome faux; lit_neg_atome vrai]])
  ; "Deux", `Quick, testez_dnf_var_libres (Sets.set_of_list ["A";"B"]) (dnf [[lit_neg_atome vrai]; [pas_var_neg_atome "A"; pas_var_neg_atome "B"]])
  ]

(* Evaluez DNF *)

let testez_evaluation_dnf (exp : verite) (don_i : interpretation) (don_p : proposition_dnf) () =
  let res = evaluez_dnf don_i don_p in
  Alcotest.(check bool) "evaluation" exp res

let appliquez_don_dnf (nom : string) (p : proposition_dnf) =
  List.map (fun (f,s) -> nom ^ " - " ^ s, `Quick, f p)

(* A + (B . C) + f *)
let suite_evaluation_dnf_0 = appliquez_don_dnf "A+(B.C)+f" (dnf [[var_neg_atome "A"]; [var_neg_atome "B"; var_neg_atome "C"]; [lit_neg_atome faux]])
  [ testez_evaluation_dnf faux ["A", faux; "B", faux; "C", faux], "000"
  ; testez_evaluation_dnf faux ["A", faux; "B", faux; "C", vrai], "001"
  ; testez_evaluation_dnf faux ["A", faux; "B", vrai; "C", faux], "010"
  ; testez_evaluation_dnf vrai ["A", faux; "B", vrai; "C", vrai], "011"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", faux; "C", faux], "100"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", faux; "C", vrai], "101"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", vrai; "C", faux], "110"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", vrai; "C", vrai], "111"
  ]

(* A + (B . f) + (C . ¬D) *)
let suite_evaluation_dnf_1 = appliquez_don_dnf "A+(B.f)+(C.¬D)" (dnf [[var_neg_atome "A"]; [var_neg_atome "B"; lit_neg_atome faux]; [var_neg_atome "C"; pas_var_neg_atome "D"]])
  [ testez_evaluation_dnf faux ["A", faux; "B", faux; "C", faux; "D", faux], "0000"
  ; testez_evaluation_dnf faux ["A", faux; "B", faux; "C", faux; "D", vrai], "0001"
  ; testez_evaluation_dnf vrai ["A", faux; "B", faux; "C", vrai; "D", faux], "0010"
  ; testez_evaluation_dnf faux ["A", faux; "B", faux; "C", vrai; "D", vrai], "0011"
  ; testez_evaluation_dnf faux ["A", faux; "B", vrai; "C", faux; "D", faux], "0100"
  ; testez_evaluation_dnf faux ["A", faux; "B", vrai; "C", faux; "D", vrai], "0101"
  ; testez_evaluation_dnf vrai ["A", faux; "B", vrai; "C", vrai; "D", faux], "0110"
  ; testez_evaluation_dnf faux ["A", faux; "B", vrai; "C", vrai; "D", vrai], "0111"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", faux; "C", faux; "D", faux], "1000"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", faux; "C", faux; "D", vrai], "1001"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", faux; "C", vrai; "D", faux], "1010"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", faux; "C", vrai; "D", vrai], "1011"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", vrai; "C", faux; "D", faux], "1100"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", vrai; "C", faux; "D", vrai], "1101"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", vrai; "C", vrai; "D", faux], "1110"
  ; testez_evaluation_dnf vrai ["A", vrai; "B", vrai; "C", vrai; "D", vrai], "1111"
  ]

let suite_evaluation_dnf =
    suite_evaluation_dnf_0
  @ suite_evaluation_dnf_1

(* NNF au CNF *)

let testez_nnf_au_cnf =
  QCheck.Test.make ~count:1000
  nnf_arbitraire
  ( fun (nnf : proposition_nnf) ->
    let cnf = nnf_au_cnf nnf in
    let vars = nnf_var_libres nnf in
    propositions_equivalents vars evaluez_nnf evaluez_cnf nnf cnf )

let suite_nnf_au_cnf = List.map QCheck_alcotest.to_alcotest
  [ testez_nnf_au_cnf ]

(* CNF variables libres *)

let testez_cnf_var_libres (exp : varnom Sets.t) (p : proposition_cnf) () =
  let res = cnf_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_cnf_var_libres =
  [ "Sets.empty", `Quick, testez_cnf_var_libres Sets.empty (cnf [[lit_neg_atome vrai]; [lit_neg_atome faux; lit_neg_atome vrai]])
  ; "Un", `Quick, testez_cnf_var_libres (Sets.set_of_list ["A"]) (cnf [[var_neg_atome "A"]; [lit_neg_atome faux; lit_neg_atome vrai]])
  ; "Deux", `Quick, testez_cnf_var_libres (Sets.set_of_list ["A";"B"]) (cnf [[lit_neg_atome vrai]; [pas_var_neg_atome "A"; pas_var_neg_atome "B"]])
  ]

(* Evaluez CNF *)

let testez_evaluation_cnf (exp : verite) (don_i : interpretation) (don_p : proposition_cnf) () =
  let res = evaluez_cnf don_i don_p in
  Alcotest.(check bool) "evaluation" exp res

let appliquez_don_cnf (nom : string) (p : proposition_cnf) =
  List.map (fun (f,s) -> nom ^ " - " ^ s, `Quick, f p)

(* A . (B + C) . f *)
let suite_evaluation_cnf_0 = appliquez_don_cnf "A.(B+C).f" (cnf [[var_neg_atome "A"]; [var_neg_atome "B"; var_neg_atome "C"]; [lit_neg_atome faux]])
  [ testez_evaluation_cnf faux ["A", faux; "B", faux; "C", faux], "000"
  ; testez_evaluation_cnf faux ["A", faux; "B", faux; "C", vrai], "001"
  ; testez_evaluation_cnf faux ["A", faux; "B", vrai; "C", faux], "010"
  ; testez_evaluation_cnf faux ["A", faux; "B", vrai; "C", vrai], "011"
  ; testez_evaluation_cnf faux ["A", vrai; "B", faux; "C", faux], "100"
  ; testez_evaluation_cnf faux ["A", vrai; "B", faux; "C", vrai], "101"
  ; testez_evaluation_cnf faux ["A", vrai; "B", vrai; "C", faux], "110"
  ; testez_evaluation_cnf faux ["A", vrai; "B", vrai; "C", vrai], "111"
  ]

(* A . (B + f) . (C + ¬D) *)
let suite_evaluation_cnf_1 = appliquez_don_cnf "A.(B+f).(C+¬D)" (cnf [[var_neg_atome "A"]; [var_neg_atome "B"; lit_neg_atome faux]; [var_neg_atome "C"; pas_var_neg_atome "D"]])
  [ testez_evaluation_cnf faux ["A", faux; "B", faux; "C", faux; "D", faux], "0000"
  ; testez_evaluation_cnf faux ["A", faux; "B", faux; "C", faux; "D", vrai], "0001"
  ; testez_evaluation_cnf faux ["A", faux; "B", faux; "C", vrai; "D", faux], "0010"
  ; testez_evaluation_cnf faux ["A", faux; "B", faux; "C", vrai; "D", vrai], "0011"
  ; testez_evaluation_cnf faux ["A", faux; "B", vrai; "C", faux; "D", faux], "0100"
  ; testez_evaluation_cnf faux ["A", faux; "B", vrai; "C", faux; "D", vrai], "0101"
  ; testez_evaluation_cnf faux ["A", faux; "B", vrai; "C", vrai; "D", faux], "0110"
  ; testez_evaluation_cnf faux ["A", faux; "B", vrai; "C", vrai; "D", vrai], "0111"
  ; testez_evaluation_cnf faux ["A", vrai; "B", faux; "C", faux; "D", faux], "1000"
  ; testez_evaluation_cnf faux ["A", vrai; "B", faux; "C", faux; "D", vrai], "1001"
  ; testez_evaluation_cnf faux ["A", vrai; "B", faux; "C", vrai; "D", faux], "1010"
  ; testez_evaluation_cnf faux ["A", vrai; "B", faux; "C", vrai; "D", vrai], "1011"
  ; testez_evaluation_cnf vrai ["A", vrai; "B", vrai; "C", faux; "D", faux], "1100"
  ; testez_evaluation_cnf faux ["A", vrai; "B", vrai; "C", faux; "D", vrai], "1101"
  ; testez_evaluation_cnf vrai ["A", vrai; "B", vrai; "C", vrai; "D", faux], "1110"
  ; testez_evaluation_cnf vrai ["A", vrai; "B", vrai; "C", vrai; "D", vrai], "1111"
  ]

let suite_evaluation_cnf =
    suite_evaluation_cnf_0
  @ suite_evaluation_cnf_1

(* Main *)

let () =
  let open Alcotest in
  run "Propositions"
  [ "Sante", suite_sante
  ; "Proposition Variables Libres", suite_prop_var_libres
  ; "Evaluation", suite_evaluation
  ; "Proposition au NNF", suite_prop_au_nnf
  ; "Propositions Equivalents", suite_propositions_equivalents
  ; "NNF Variables Libres", suite_nnf_var_libres
  ; "Evaluation NNF", suite_evaluation_nnf
  ; "NNF au DNF", suite_nnf_au_dnf
  ; "DNF Variables Libres", suite_dnf_var_libres
  ; "Evaluation DNF", suite_evaluation_dnf
  ; "NNF au CNF", suite_nnf_au_cnf
  ; "CNF Variables Libres", suite_cnf_var_libres
  ; "Evaluation CNF", suite_evaluation_cnf
  ]
