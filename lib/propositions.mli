(** Les trucs pour la logique propositionnel *)

(** Une valeur booleene *)
type verite = bool

(** La verite vrai *)
val vrai : verite

(** La verite faux *)
val faux : verite

(** Le nom d'une variable *)
type varnom = string

(** Un atome d'une proposition. Soit un literal, soit une variable *)
type atome =
  | Lit of verite
  | Var of varnom

(** Les propositions brutes *)

(** Une proposition logique *)
type proposition =
  | Atome of atome
  | Ou of proposition Pasvide.pas_vide
  | Et of proposition Pasvide.pas_vide
  | Pas of proposition
  | Impl of proposition * proposition
  | BiImpl of proposition * proposition

val proposition_arbitraire : proposition QCheck.arbitrary

val string_of_proposition : proposition -> string

(** Trouve les noms des variables libres dans une proposition *)
val prop_var_libres : proposition -> varnom Sets.set

(** Interpretations *)

(** Un "mapping" de noms des variables a la valeur booleene *)
type interpretation = (varnom * verite) list

(** Trouvez la valeure d'une variable dans une interpretation *)
val interpretation_cherche : varnom -> interpretation -> verite option

(** Modifiez une interpretation pour definir un valuer pour un nom *)
val interpretation_ajoute :
  varnom -> verite -> interpretation -> interpretation

(** Comparez des propositions utilisant leurs fonctions d'evaluation pour voir si ils sont equivalents *)
val propositions_equivalents : varnom Sets.set -> (interpretation -> 'a -> bool) -> (interpretation -> 'b -> bool) -> 'a -> 'b -> bool

(** Les propositions simples *)

(** Une proposition simple, sans implications *)
type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple Pasvide.pas_vide
  | Et of proposition_simple Pasvide.pas_vide
  | Pas of proposition_simple

val string_of_simple : proposition_simple -> string

(** Mettez une proposition brute en proposition simple, sans implication *)
val prop_au_simple : proposition -> proposition_simple

(** Evaluez une proposition avec une interpretation *)
val evaluez : interpretation -> proposition -> verite

(** Les propositions en forme NNF *)

(** Un atome qui peut etre negatif *)
type neg_atome =
  | AtomeLit of verite
  | AtomeVar of varnom
  | PasAtomeVar of varnom

(** Une proposition en NNF ("Negation Normal Form"), ou les "Pas"s sont sur les atomes seuls *)
type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf Pasvide.pas_vide
  | Et of proposition_nnf Pasvide.pas_vide

val nnf_arbitraire : proposition_nnf QCheck.arbitrary

val string_of_nnf : proposition_nnf -> string

(** Mettez une proposition simple en forme NNF *)
val simple_au_nnf : proposition_simple -> proposition_nnf

(** Trouve les noms des variables libres dans une proposition en forme NNF *)
val nnf_var_libres : proposition_nnf -> varnom Sets.set

(** Evaluez une proposition en forme NNF *)
val evaluez_nnf : interpretation -> proposition_nnf -> verite

(** Les propositions en forme DNF et CNF *)

(** Une terme d'une proposition en DNF *)
type terme_dnf = neg_atome Pasvide.pas_vide

(** Une proposition en DNF ("Disjunctive Normal Form"), comme "(A . B) + C + (D . E . F)" *)
type proposition_dnf = terme_dnf Pasvide.pas_vide

val dnf_arbitraire : proposition_dnf QCheck.arbitrary

val string_of_dnf : proposition_dnf -> string

(** Une terme d'une proposition en CNF *)
type terme_cnf = neg_atome Pasvide.pas_vide

(** Une proposition en CNF ("Conjunctive Normal Form"), comme "(A + B) . C . (D + E + F)" *)
type proposition_cnf = terme_cnf Pasvide.pas_vide

val cnf_arbitraire : proposition_cnf QCheck.arbitrary

val string_of_cnf : proposition_cnf -> string

(** Mettez une proposition en forme NNF en forme DNF *)
val nnf_au_dnf : proposition_nnf -> proposition_dnf

(** Trouve les noms des variables libres dans une proposition en forme DNF *)
val dnf_var_libres : proposition_dnf -> varnom Sets.set

(** Evaluez une proposition en forme DNF *)
val evaluez_dnf : interpretation -> proposition_dnf -> verite

(** Mettez une proposition en forme NNF en forme CNF *)
val nnf_au_cnf : proposition_nnf -> proposition_cnf

(** Trouve les noms des variables libres dans une proposition en forme CNF *)
val cnf_var_libres : proposition_cnf -> varnom Sets.set

(** Evaluez une proposition en forme CNF *)
val evaluez_cnf : interpretation -> proposition_cnf -> verite
