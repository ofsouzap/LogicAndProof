(** Le calcul des sequents *)

open Nicelib
open Propositions

(** Un sequent d'hypotheses et conclusions *)
type sequent = proposition Sets.t * proposition Sets.t

val string_of_sequent : sequent -> string
