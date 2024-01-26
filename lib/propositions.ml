type verite = bool
(** Un valeur booleen *)

type varnom = string
(** Le nom d'une variable *)

type atome =
  | Lit of verite
  | Var of varnom
(** Un atome d'une proposition. Soit un litteral, soit une variable *)

type proposition =
  | Atome of atome
  | Ou of proposition * proposition
  | Et of proposition * proposition
  | Pas of proposition
  | Impl of proposition * proposition
  | BiImpl of proposition * proposition
(** Une proposition logique *)

type interpretation = (varnom * verite) list
(** Un "mapping" de noms des variables au valeur booleen *)

let rec interpretation_cherche (nom : varnom) (i : interpretation) : verite option = match i with
  | [] -> None
  | ((hnom, hval)::ts) -> if hnom = nom
    then Some hval
    else interpretation_cherche nom ts
(** Trouvez le valuer d'un variable dans un interpretation *)

let interpretation_ajoute (nom : varnom) (b : verite) (i : interpretation) : interpretation =
  (nom, b) :: i
(** Modifiez un interpretation pour definir un valuer pour un nom *)

type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple * proposition_simple
  | Et of proposition_simple * proposition_simple
  | Pas of proposition_simple
(** Un proposition simple, pas avec les implications *)

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
(** Mettez un proposition brut en proposition simple, sans implications *)

type neg_atome =
| Atome of atome
| PasAtome of atome
(** Un atome qui peut etre negatif *)

type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf * proposition_nnf
  | Et of proposition_nnf * proposition_nnf
(** Un proposition en NNF ("Negation Normal Form"), ou les "Pas"s sont sur les atomes seuls *)

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
(** Mettez un proposition simple en forme NNF *)

(** TODO - faiez le truque pour transformer les proposition en CNF et en DNF *)
