type verite = bool
let vrai : verite = true
let faux : verite = false

type varnom = string
type atome =
  | Lit of verite
  | Var of varnom

type proposition =
  | Atome of atome
  | Ou of proposition * proposition
  | Et of proposition * proposition
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
  | Ou of proposition_simple * proposition_simple
  | Et of proposition_simple * proposition_simple
  | Pas of proposition_simple

let rec prop_au_simple (p : proposition) : proposition_simple = match p with
  | Atome a -> Atome a
  | Ou (a, b) -> Ou (prop_au_simple a, prop_au_simple b)
  | Et (a, b) -> Et (prop_au_simple a, prop_au_simple b)
  | Pas x -> Pas (prop_au_simple x)
  | Impl (a, b) ->
    let a' = prop_au_simple a in
    let b' = prop_au_simple b in
      Ou (Pas a', b')
  | BiImpl (a, b) ->
    let a' = prop_au_simple a in
    let b' = prop_au_simple b in
      Ou (
        Et (a', b'),
        Et (Pas a', Pas b')
      )

let rec evaluez_simple (i : interpretation) = function
  | Atome (Lit b) -> b
  | Atome (Var n) -> (match interpretation_cherche n i with | None -> false | Some b -> b)
  | Ou (a, b) -> evaluez_simple i a || evaluez_simple i b
  | Et (a, b) -> evaluez_simple i a && evaluez_simple i b
  | Pas x -> not (evaluez_simple i x)

let evaluez (i : interpretation) (p : proposition) : verite =
  let p' = prop_au_simple p in
  evaluez_simple i p'

type neg_atome =
| Atome of atome
| PasAtome of atome

type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf * proposition_nnf
  | Et of proposition_nnf * proposition_nnf

let rec simple_au_nnf (p : proposition_simple) : proposition_nnf = match p with
  | Atome a -> Atome (Atome a)
  | Ou (a, b) -> Ou (simple_au_nnf a, simple_au_nnf b)
  | Et (a, b) -> Et (simple_au_nnf a, simple_au_nnf b)
  | Pas (Ou (a, b)) -> Et (
    simple_au_nnf (Pas a),
    simple_au_nnf (Pas b)
  )
  | Pas (Et (a, b)) -> Ou (
    simple_au_nnf (Pas a),
    simple_au_nnf (Pas b)
  )
  | Pas (Atome a) -> Atome (PasAtome a)
  | Pas (Pas x) -> simple_au_nnf x

(** TODO - faisez le truc pour transformer les proposition en CNF et en DNF *)
