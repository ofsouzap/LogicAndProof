(** Le calcul des sequents *)

open Nicelib
open Propositions

(** Un sequent d'hypotheses et conclusions *)
type sequent

(** Creez un sequent *)
val make : proposition Sets.t * proposition Sets.t -> sequent

(** Creez un sequent avec les hypotheses et conclusions en listes *)
val make_listes : proposition list * proposition list -> sequent

(** Prennez les hypotheses d'un sequent *)
val hypotheses : sequent -> proposition Sets.t

(** Prennez les conclusions d'un sequent *)
val conclusions : sequent -> proposition Sets.t

val string_of_sequent : sequent -> string

(** Regardez si un sequent est un sequent axiome *)
val sequent_est_axiome : sequent -> bool

(** Creez les precedents possibles avec la regle sequente negation a gauche *)
val prec_neg_gauche : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente negation a droite *)
val prec_neg_droit : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente conjonction a gauche *)
val prec_conj_gauche : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente conjonction a droite *)
val prec_conj_droit : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente disjonction a gauche *)
val prec_disj_gauche : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente disjonction a droite *)
val prec_disj_droit : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente implication a gauche *)
val prec_impl_gauche : sequent -> sequent Sets.t Seq.t

(** Creez les precedents possibles avec la regle sequente implication a droite *)
val prec_impl_droit : sequent -> sequent Sets.t Seq.t
