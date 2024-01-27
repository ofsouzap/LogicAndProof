type verite = bool
(** Une valeur booleene *)

val vrai : verite
(** La verite vrai *)

val faux : verite
(** La verite faux *)

type varnom = string
(** Le nom d'une variable *)

type atome =
  | Lit of verite
  | Var of varnom
(** Un atome d'une proposition. Soit un literal, soit une variable *)

type proposition =
  | Atome of atome
  | Ou of proposition Pasvide.pas_vide
  | Et of proposition Pasvide.pas_vide
  | Pas of proposition
  | Impl of proposition * proposition
  | BiImpl of proposition * proposition
(** Une proposition logique *)

type interpretation = (varnom * verite) list
(** Un "mapping" de noms des variables a la valeur booleene *)

val interpretation_cherche : varnom -> interpretation -> verite option
(** Trouvez la valeure d'une variable dans une interpretation *)

val interpretation_ajoute :
  varnom -> verite -> interpretation -> interpretation
(** Modifiez une interpretation pour definir un valuer pour un nom *)

type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple Pasvide.pas_vide
  | Et of proposition_simple Pasvide.pas_vide
  | Pas of proposition_simple
(** Une proposition simple, sans implications *)

val prop_au_simple : proposition -> proposition_simple
(** Mettez une proposition brute en proposition simple, sans implication *)

val evaluez : interpretation -> proposition -> verite
(** Evaluez une proposition avec une interpretation *)

type neg_atome =
  | Atome of atome
  | PasAtome of atome
(** Un atome qui peut etre negatif *)

type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf Pasvide.pas_vide
  | Et of proposition_nnf Pasvide.pas_vide
(** Une proposition en NNF ("Negation Normal Form"), ou les "Pas"s sont sur les atomes seuls *)

val simple_au_nnf : proposition_simple -> proposition_nnf
(** Mettez une proposition simple en forme NNF *)
