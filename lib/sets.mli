(* Les "sets" (les collections) *)

type 'a set
(* Un set *)

val formateur_int_set : Format.formatter -> int set -> unit

val formateur_string_set : Format.formatter -> string set -> unit

val vide : 'a set
(* Le set vide *)

val singleton : 'a -> 'a set
(* Le set avec un seul element *)

val ajoutez : 'a -> 'a set -> 'a set
(* Ajoutez un element au set *)

val membre : 'a -> 'a set -> bool
(* Regardez si un element et dans un set *)

val compte : 'a set -> int
(* Combien d'elements a le set *)

val intersection : 'a set -> 'a set -> 'a set
(* Produisez l'intersection des sets *)

val union : 'a set -> 'a set -> 'a set
(* Produisez l'union des sets *)

val pareil : 'a set -> 'a set -> bool
(* Comparez deux sets *)

val list_of_set : 'a set -> 'a list
(* Donnez une liste des elements d'un set *)

val set_of_list : 'a list -> 'a set
(* Construirez un set des elements d'une liste *)

val set_gen_max : int -> 'a QCheck.Gen.t -> 'a set QCheck.Gen.t

val set_arbitraire : 'a QCheck.arbitrary -> 'a set QCheck.arbitrary

val set_arbitraire_max : int -> 'a QCheck.arbitrary -> 'a set QCheck.arbitrary
