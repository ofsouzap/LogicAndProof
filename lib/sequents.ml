open Sets
open Propositions

type sequent = proposition set * proposition set

let string_of_sequent (hs, cs) =
    "(" ^ Utils.intercalez_str "),(" (List.map string_of_proposition (list_of_set hs)) ^ ")"
  ^ "⊢"
  ^ "(" ^ Utils.intercalez_str "),(" (List.map string_of_proposition (list_of_set cs)) ^ ")"
