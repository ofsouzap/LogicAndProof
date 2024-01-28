open LogicAndProof.Sets
open LogicAndProof.Pasvide
open LogicAndProof.Propositions
open LogicAndProof.PropositionsAbr

let varnom_set : ('a set) Alcotest.testable = Alcotest.testable formateur_string_set pareil

let test_sante () =
  let exp = 5 in
  let res = 3 + 2 in
  Alcotest.(check int) "l'addition correcte" res exp

let suite_sante =
  [ "l'addition", `Quick, test_sante
  ]

(* Propositions variables libres *)

let testez_prop_var_libres (exp : varnom set) (p : proposition) () =
  let res = prop_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_prop_var_libres =
  [ "Vide", `Quick, testez_prop_var_libres vide (Impl ((lit_prop vrai), (ou_prop [lit_prop faux; lit_prop vrai])))
  ; "Un", `Quick, testez_prop_var_libres (set_of_list ["A"]) (Impl ((var_prop "A"), (ou_prop [lit_prop faux; lit_prop vrai])))
  ; "Deux", `Quick, testez_prop_var_libres (set_of_list ["A";"B"]) (Impl ((var_prop "A"), (ou_prop [var_prop "B"; var_prop "B"])))
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

(* Simple au NNF *)

(* TODO *)

(* NNF variables libres *)

let testez_nnf_var_libres (exp : varnom set) (p : proposition_nnf) () =
  let res = nnf_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_nnf_var_libres =
  [ "Vide", `Quick, testez_nnf_var_libres vide (ou_nnf [lit_nnf vrai; ou_nnf [lit_nnf faux; lit_nnf vrai]])
  ; "Un", `Quick, testez_nnf_var_libres (set_of_list ["A"]) (ou_nnf [lit_nnf vrai; ou_nnf [pas_var_nnf "A"; lit_nnf vrai]])
  ; "Deux", `Quick, testez_nnf_var_libres (set_of_list ["A";"B"]) (ou_nnf [var_nnf "A"; ou_nnf [pas_var_nnf "B"; var_nnf "B"]])
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

(* TODO *)

(* DNF variables libres *)

let testez_dnf_var_libres (exp : varnom set) (p : proposition_dnf) () =
  let res = dnf_var_libres p in
  Alcotest.check varnom_set "" exp res

let suite_dnf_var_libres =
  [ "Vide", `Quick, testez_dnf_var_libres vide (dnf [[lit_neg_atome vrai]; [lit_neg_atome faux; lit_neg_atome vrai]])
  ; "Un", `Quick, testez_dnf_var_libres (set_of_list ["A"]) (dnf [[var_neg_atome "A"]; [lit_neg_atome faux; lit_neg_atome vrai]])
  ; "Deux", `Quick, testez_dnf_var_libres (set_of_list ["A";"B"]) (dnf [[lit_neg_atome vrai]; [pas_var_neg_atome "A"; pas_var_neg_atome "B"]])
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
let suite_evaluation_dnf_1 = appliquez_don_dnf "A+(B.f)+(C+¬D)" (dnf [[var_neg_atome "A"]; [var_neg_atome "B"; lit_neg_atome faux]; [var_neg_atome "C"; pas_var_neg_atome "D"]])
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

(* Main *)

let () =
  let open Alcotest in
  run "Propositions"
  [ "Sante", suite_sante
  ; "Proposition Variables Libres", suite_prop_var_libres
  ; "Evaluation", suite_evaluation
  ; "NNF Variables Libres", suite_nnf_var_libres
  ; "Evaluation NNF", suite_evaluation_nnf
  ; "DNF Variables Libres", suite_dnf_var_libres
  ; "Evaluation DNF", suite_evaluation_dnf
  ]
