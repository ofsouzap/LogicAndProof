(** Les "sets" (les collections) *)

(** Un set *)
type 'a set

val formateur_int_set : Format.formatter -> int set -> unit

val formateur_string_set : Format.formatter -> string set -> unit

(** Le set vide *)
val vide : 'a set

(** Le set avec un seul element *)
val singleton : 'a -> 'a set

(** Ajoutez un element au set *)
val ajoutez : 'a -> 'a set -> 'a set

(** Regardez si un element et dans un set *)
val membre : 'a -> 'a set -> bool

(** Combien d'elements a le set *)
val compte : 'a set -> int

(** Produisez l'intersection des sets *)
val intersection : 'a set -> 'a set -> 'a set

(** Produisez l'union des sets *)
val union : 'a set -> 'a set -> 'a set

(** Comparez deux sets *)
val pareil : 'a set -> 'a set -> bool

(** Donnez une liste des elements d'un set *)
val list_of_set : 'a set -> 'a list

(** Construirez un set des elements d'une liste *)
val set_of_list : 'a list -> 'a set

val set_gen_max : int -> 'a QCheck.Gen.t -> 'a set QCheck.Gen.t

val set_arbitraire : 'a QCheck.arbitrary -> 'a set QCheck.arbitrary

val set_arbitraire_max : int -> 'a QCheck.arbitrary -> 'a set QCheck.arbitrary
