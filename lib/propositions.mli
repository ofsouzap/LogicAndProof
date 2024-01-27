type verite = bool
(** Un valeur booleen *)

val vrai : verite
(** La verite vrai *)

val faux : verite
(** La verite faux *)

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

val interpretation_cherche : varnom -> interpretation -> verite option
(** Trouvez le valuer d'un variable dans un interpretation *)

val interpretation_ajoute :
  varnom -> verite -> interpretation -> interpretation
(** Modifiez un interpretation pour definir un valuer pour un nom *)

type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple * proposition_simple
  | Et of proposition_simple * proposition_simple
  | Pas of proposition_simple
(** Un proposition simple, pas avec les implications *)

val prop_au_simple : proposition -> proposition_simple
(** Mettez un proposition brut en proposition simple, sans implications *)

val evaluez : interpretation -> proposition -> verite
(** Evaluez un proposition avec un interpretation *)

type neg_atome = Atome of atome | PasAtome of atome
(** Un atome qui peut etre negatif *)

type proposition_nnf =
    Atome of neg_atome
  | Ou of proposition_nnf * proposition_nnf
  | Et of proposition_nnf * proposition_nnf
(** Un proposition en NNF ("Negation Normal Form"), ou les "Pas"s sont sur les atomes seuls *)

val simple_au_nnf : proposition_simple -> proposition_nnf
(** Mettez un proposition simple en forme NNF *)
