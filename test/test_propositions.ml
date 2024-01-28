open LogicAndProof.Pasvide
open LogicAndProof.Propositions

let test_sante () =
  let exp = 5 in
  let res = 3 + 2 in
  Alcotest.(check int) "l'addition correcte" res exp

let suite_sante =
  [ "l'addition", `Quick, test_sante
  ]

(* Evaluation *)

let testez_evaluation (exp : verite) (don_i : interpretation) (don_p : proposition) () =
  let res = evaluez don_i don_p in
  Alcotest.(check bool) "evaluation" exp res

let rec appliquez_don_prop (nom : string) (p : proposition) (xs : ((proposition -> unit -> unit) * string) list) = match xs with
  | [] -> []
  | (f,s)::ts -> (nom ^ " - " ^ s, `Quick, f p) :: appliquez_don_prop nom p ts

(* (P . Q) -> P *)
let suite_evaluation_0 = appliquez_don_prop "(P.Q)->P" (Impl (Et (pas_vide_of_list [((Atome (Var "P")) : proposition); (Atome (Var "Q"))]), Atome (Var "P")))
  [ testez_evaluation vrai ["P", faux; "Q", faux], "00"
  ; testez_evaluation vrai ["P", faux; "Q", vrai], "01"
  ; testez_evaluation vrai ["P", vrai; "Q", faux], "10"
  ; testez_evaluation vrai ["P", vrai; "Q", vrai], "11"
  ]

(* (P + Q) -> P *)
let suite_evaluation_1 = appliquez_don_prop "(P+Q)->P" (Impl (Ou (pas_vide_of_list [(Atome (Var "P") : proposition); Atome (Var "Q")]), Atome (Var "P")))
  [ testez_evaluation vrai ["P", faux; "Q", faux], "00"
  ; testez_evaluation faux ["P", faux; "Q", vrai], "01"
  ; testez_evaluation vrai ["P", vrai; "Q", faux], "10"
  ; testez_evaluation vrai ["P", vrai; "Q", vrai], "11"
  ]

(* P <-> (t . (Q + R)) *)
let suite_evaluation_2 = appliquez_don_prop "P<->(t.(Q+R))" (BiImpl ((Atome (Var "P")), (Et (paire (Atome (Lit vrai) : proposition) (Ou (paire (Atome (Var "Q") : proposition) (Atome (Var "R"))))))))
  [ testez_evaluation vrai ["P", faux; "Q", faux; "R", faux], "000"
  ; testez_evaluation faux ["P", faux; "Q", faux; "R", vrai], "001"
  ; testez_evaluation faux ["P", faux; "Q", vrai; "R", faux], "010"
  ; testez_evaluation faux ["P", faux; "Q", vrai; "R", vrai], "011"
  ; testez_evaluation faux ["P", vrai; "Q", faux; "R", faux], "100"
  ; testez_evaluation vrai ["P", vrai; "Q", faux; "R", vrai], "101"
  ; testez_evaluation vrai ["P", vrai; "Q", vrai; "R", faux], "110"
  ; testez_evaluation vrai ["P", vrai; "Q", vrai; "R", vrai], "111"
  ]

let suite_evaluation =
    suite_evaluation_0
  @ suite_evaluation_1
  @ suite_evaluation_2

(* Proposition au Simple *)

(* TODO *)

(* Simple au NNF *)

(* TODO *)

(* NNF au DNF *)

(* TODO *)

(* Main *)

let () =
  let open Alcotest in
  run "Propositions"
  [ "Sante", suite_sante
  ; "Evaluation", suite_evaluation
  ]
