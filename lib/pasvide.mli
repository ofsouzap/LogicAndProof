type 'a pas_vide =
  | Cons of 'a * 'a pas_vide
  | Feui of 'a
(** Une liste qui n'est pas vide *)

val singleton : 'a -> 'a pas_vide
(** Cree une pas-vide avec un seul valeur *)

val apposez : 'a -> 'a pas_vide -> 'a pas_vide
(** Apposez un valeur au fin d'une pas-vide *)

val ajoutez : 'a -> 'a pas_vide -> 'a pas_vide
(** Ajoutez un valeur au debut d'une pas-vide *)

val tete : 'a pas_vide -> 'a
(** Prennez le premier valeur d'un pas-vide *)

val tail : 'a pas_vide -> 'a pas_vide option
(** Essayez de prendre le "tail" d'une pas-vide. Sinon, on retour None *)

val reverse : 'a pas_vide -> 'a pas_vide
(** Reversez une pas-vide *)

val pas_vide_of_list : 'a list -> 'a pas_vide
(** Construisez la pas-vide d'une liste. Si la liste est vide, ca echoue *)

val list_of_pas_vide : 'a pas_vide -> 'a list
(** Construisez la liste d'une pas-vide *)

val map_rev : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide
(** Mappez une pas-vide avec une mappage. Le resultat est reverse *)

val map : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide
(** Mappez une pas-vide avec une mappage *)

val zip_rev : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide
(** Fermez les deux pas-vides avex eux-memes. Le resultat est reverse *)

val zip : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide
(** Fermez les deux pas-vides avex eux-memes *)

val tous : ('a -> bool) -> 'a pas_vide -> bool
(** Verifiez si tous les elements dans une pas-vide satisfont un predicat *)

val quelque : ('a -> bool) -> 'a pas_vide -> bool
(** Verifiez si il y en a un element dans une pas-vide qui satisfait un predicat *)
