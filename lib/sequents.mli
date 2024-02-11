(** Le calcul des sequents *)

open Sets
open Propositions

(** Un sequent d'hypotheses et conclusions *)
type sequent = proposition set * proposition set

val string_of_sequent : sequent -> string
