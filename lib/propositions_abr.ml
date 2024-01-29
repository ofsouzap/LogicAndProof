open Propositions

(* NegAtome *)

let lit_neg_atome (b : bool) : neg_atome = Atome (Lit b)
let var_neg_atome (nom : string) : neg_atome = Atome (Var nom)
let pas_var_neg_atome (nom : string) : neg_atome = PasAtome (Var nom)

(* Propositions Brutes *)

let lit_prop (b : bool) : proposition = Atome (Lit b)
let var_prop (nom : string) : proposition = Atome (Var nom)
let ou_prop (xs : proposition list) : proposition = Ou (Pasvide.pas_vide_of_list xs)
let et_prop (xs : proposition list) : proposition = Et (Pasvide.pas_vide_of_list xs)

(* NNF *)

let lit_nnf (b : bool) : proposition_nnf = Atome (Atome (Lit b))
let var_nnf (nom : string) : proposition_nnf = Atome (Atome (Var nom))
let pas_lit_nnf (b : bool) : proposition_nnf = Atome (PasAtome (Lit b))
let pas_var_nnf (nom : string) : proposition_nnf = Atome (PasAtome (Var nom))
let ou_nnf (xs : proposition_nnf list) : proposition_nnf = Ou (Pasvide.pas_vide_of_list xs)
let et_nnf (xs : proposition_nnf list) : proposition_nnf = Et (Pasvide.pas_vide_of_list xs)

(* DNF *)

let dnf (xs : neg_atome list list) : proposition_dnf =
  Pasvide.pas_vide_of_list (List.map Pasvide.pas_vide_of_list xs)
