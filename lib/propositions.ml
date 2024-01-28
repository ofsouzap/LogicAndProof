type verite = bool
let vrai : verite = true
let faux : verite = false

type varnom = string
type atome =
  | Lit of verite
  | Var of varnom

type proposition =
  | Atome of atome
  | Ou of proposition Pasvide.pas_vide
  | Et of proposition Pasvide.pas_vide
  | Pas of proposition
  | Impl of proposition * proposition
  | BiImpl of proposition * proposition

type interpretation = (varnom * verite) list

let rec interpretation_cherche (nom : varnom) (i : interpretation) : verite option = match i with
  | [] -> None
  | ((hnom, hval)::ts) -> if hnom = nom
    then Some hval
    else interpretation_cherche nom ts

let interpretation_ajoute (nom : varnom) (b : verite) (i : interpretation) : interpretation =
  (nom, b) :: i

type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple Pasvide.pas_vide
  | Et of proposition_simple Pasvide.pas_vide
  | Pas of proposition_simple

let rec prop_au_simple (p : proposition) : proposition_simple = match p with
  | Atome a -> Atome a
  | Ou xs -> Ou (Pasvide.map prop_au_simple xs)
  | Et xs -> Et (Pasvide.map prop_au_simple xs)
  | Pas x -> Pas (prop_au_simple x)
  | Impl (a, b) ->
    let a' = prop_au_simple a in
    let b' = prop_au_simple b in
      Ou (Pasvide.paire (Pas a') b')
  | BiImpl (a, b) ->
    let a' = prop_au_simple a in
    let b' = prop_au_simple b in
      Ou (Pasvide.paire
        (Et (Pasvide.paire a' b'))
        (Et (Pasvide.paire (Pas a') (Pas b')))
      )

let rec evaluez_simple (i : interpretation) = function
  | Atome (Lit b) -> b
  | Atome (Var n) -> (match interpretation_cherche n i with | None -> false | Some b -> b)
  | Ou xs -> Pasvide.quelque (evaluez_simple i) xs
  | Et xs -> Pasvide.tous (evaluez_simple i) xs
  | Pas x -> not (evaluez_simple i x)

let evaluez (i : interpretation) (p : proposition) : verite =
  let p' = prop_au_simple p in
  evaluez_simple i p'

type neg_atome =
  | Atome of atome
  | PasAtome of atome

type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf Pasvide.pas_vide
  | Et of proposition_nnf Pasvide.pas_vide

let rec simple_au_nnf (p : proposition_simple) : proposition_nnf = match p with
  | Atome a -> Atome (Atome a)
  | Ou xs -> Ou (Pasvide.map simple_au_nnf xs)
  | Et xs -> Et (Pasvide.map simple_au_nnf xs)
  | Pas (Ou xs) ->
    let mapf x : proposition_nnf = simple_au_nnf (Pas x) in
    Et (Pasvide.map mapf xs)
  | Pas (Et xs) ->
    let mapf x : proposition_nnf = simple_au_nnf (Pas x) in
    Ou (Pasvide.map mapf xs)
  | Pas (Atome a) -> Atome (PasAtome a)
  | Pas (Pas x) -> simple_au_nnf x

type terme_dnf = neg_atome Pasvide.pas_vide

type proposition_dnf = terme_dnf Pasvide.pas_vide

let fussionnez_deux_dnf (x : proposition_dnf) (y : proposition_dnf) : proposition_dnf =
  let zs = Pasvide.prod_cartesian x y in
  Pasvide.map_rev
    (fun (a,b) -> Pasvide.enchainez a b)
    zs

let fusionnez_dnf (xs : proposition_dnf Pasvide.pas_vide) : proposition_dnf = match xs with
  | Feui x -> x
  | Cons (xh,xts) -> Pasvide.foldl fussionnez_deux_dnf xh xts

let aplatissez_dnf (xs : proposition_dnf Pasvide.pas_vide) : proposition_dnf = match xs with
  | Feui x -> x
  | Cons (xh, xts) -> Pasvide.foldl Pasvide.enchainez xh xts

let rec nnf_au_dnf (p : proposition_nnf) : proposition_dnf = match p with
  | Atome a -> Pasvide.singleton (Pasvide.singleton a)
  | Ou xs ->
    let xs' = Pasvide.map nnf_au_dnf xs in
    aplatissez_dnf xs'
  | Et xs ->
    let xs' = Pasvide.map nnf_au_dnf xs in
    fusionnez_dnf xs'

(** TODO - faisez le truc pour transformer les proposition en CNF *)
