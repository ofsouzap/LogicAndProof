type 'a pas_vide =
  | Cons of 'a * 'a pas_vide
  | Feui of 'a
(** Une liste qui n'est pas vide *)

val singleton : 'a -> 'a pas_vide
(** Cree un pas-vide avec une seule valeur *)

val paire : 'a -> 'a -> 'a pas_vide
(** Cree un pas-vide avec deux valeurs *)

val apposez : 'a -> 'a pas_vide -> 'a pas_vide
(** Apposez une valeur au fin d'un pas-vide *)

val ajoutez : 'a -> 'a pas_vide -> 'a pas_vide
(** Ajoutez une valeur au debut d'un pas-vide *)

val pas_vide_arbitraire : 'a QCheck.Gen.t -> 'a pas_vide QCheck.Gen.t

val enchainez : 'a pas_vide -> 'a pas_vide -> 'a pas_vide
(** Enchainez deux pas-vides *)

val (@:) : 'a pas_vide -> 'a pas_vide -> 'a pas_vide
(** Enchainez deux pas-vides *)

val tete : 'a pas_vide -> 'a
(** Prenez la premiere valeur d'un pas-vide *)

val tail : 'a pas_vide -> 'a pas_vide option
(** Essayez de prendre le "tail" d'un pas-vide. Sinon on retourne None *)

val renverse : 'a pas_vide -> 'a pas_vide
(** Renversez un pas-vide *)

val pas_vide_of_list : 'a list -> 'a pas_vide
(** Construisez la pas-vide d'une liste. Si la liste est vide, cela echoue *)

val list_of_pas_vide : 'a pas_vide -> 'a list
(** Construisez la liste d'un pas-vide *)

val foldl : ('b -> 'a -> 'b) -> 'b -> 'a pas_vide -> 'b
(** Faisez une operation "fold" sur un pas-vide *)

val foldr : ('a -> 'b -> 'b) -> 'b -> 'a pas_vide -> 'b
(** Faisez une operation "fold" sur un pas-vide *)

val map_rev : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide
(** Mappez un pas-vide avec une mappage. Le resultat est renverse *)

val map : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide
(** Mappez un pas-vide avec une mappage *)

val zip_rev : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide
(** Fermez les deux pas-vides avex eux-memes. Le resultat est renverse *)

val zip : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide
(** Fermez les deux pas-vides avex eux-memes *)

val tous : ('a -> bool) -> 'a pas_vide -> bool
(** Verifiez si tous les elements dans un pas-vide satisfont un predicat *)

val quelque : ('a -> bool) -> 'a pas_vide -> bool
(** Verifiez si il y en a un element dans un pas-vide qui satisfait un predicat *)

val prod_cartesian : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide
(** Construisez tous les paires possibles. C'est un produit cartesian *)
