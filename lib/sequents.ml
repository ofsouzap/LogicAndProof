open Utils
open Nicelib
open Propositions

type sequent = proposition Sets.t * proposition Sets.t

let string_of_sequent (hs, cs) =
    "(" ^ intercalez_str "),(" (List.map string_of_proposition (Sets.list_of_set hs)) ^ ")"
  ^ "⊢"
  ^ "(" ^ intercalez_str "),(" (List.map string_of_proposition (Sets.list_of_set cs)) ^ ")"
