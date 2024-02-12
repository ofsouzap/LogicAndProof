(** Les lists qui ne sont pas vide *)

(** Une liste qui n'est pas vide *)
type 'a pas_vide =
  | Cons of 'a * 'a pas_vide
  | Feui of 'a

(** Cree un pas-vide avec une seule valeur *)
val singleton : 'a -> 'a pas_vide

(** Cree un pas-vide avec deux valeurs *)
val paire : 'a -> 'a -> 'a pas_vide

(** Apposez une valeur au fin d'un pas-vide *)
val apposez : 'a -> 'a pas_vide -> 'a pas_vide

(** Ajoutez une valeur au debut d'un pas-vide *)
val ajoutez : 'a -> 'a pas_vide -> 'a pas_vide

(** Enchainez deux pas-vides *)
val enchainez : 'a pas_vide -> 'a pas_vide -> 'a pas_vide

(** Enchainez deux pas-vides *)
val (@:) : 'a pas_vide -> 'a pas_vide -> 'a pas_vide

(** Intercalez une pas-vide des strings avec un string *)
val intercalez : string -> string pas_vide -> string

(** Prenez la premiere valeur d'un pas-vide *)
val tete : 'a pas_vide -> 'a

(** Essayez de prendre le "tail" d'un pas-vide. Sinon on retourne None *)
val tail : 'a pas_vide -> 'a pas_vide option

(** Renversez un pas-vide *)
val renverse : 'a pas_vide -> 'a pas_vide

(** Construisez la pas-vide d'une liste. Si la liste est vide, cela echoue *)
val pas_vide_of_list : 'a list -> 'a pas_vide

(** Construisez la liste d'un pas-vide *)
val list_of_pas_vide : 'a pas_vide -> 'a list

(** Faisez une operation "fold" sur un pas-vide *)
val foldl : ('b -> 'a -> 'b) -> 'b -> 'a pas_vide -> 'b

(** Faisez une operation "fold" sur un pas-vide *)
val foldr : ('a -> 'b -> 'b) -> 'b -> 'a pas_vide -> 'b

(** Aplatissez les pas-vides *)
val aplatissez : 'a pas_vide pas_vide -> 'a pas_vide

(** Mappez un pas-vide avec une mappage. Le resultat est renverse *)
val map_rev : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide

(** Mappez un pas-vide avec une mappage *)
val map : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide

(** Fermez les deux pas-vides avex eux-memes. Le resultat est renverse *)
val zip_rev : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide

(** Fermez les deux pas-vides avex eux-memes *)
val zip : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide

(** Verifiez si tous les elements dans un pas-vide satisfont un predicat *)
val tous : ('a -> bool) -> 'a pas_vide -> bool

(** Verifiez si il y en a un element dans un pas-vide qui satisfait un predicat *)
val quelque : ('a -> bool) -> 'a pas_vide -> bool

(** Le compte *)
val compte : 'a pas_vide -> int

(** Construisez tous les paires possibles. C'est un produit cartesian *)
val prod_cartesian : 'a pas_vide -> 'b pas_vide -> ('a * 'b) pas_vide

val pas_vide_arbitraire : 'a QCheck.arbitrary -> 'a pas_vide QCheck.arbitrary

val pas_vide_arbitraire_n : int -> 'a QCheck.arbitrary -> 'a pas_vide QCheck.arbitrary

(** Instances typeclass pour nicelib *)

(** Fmap pour les pas-vides *)
val ( <$>.|. ) : ('a -> 'b) -> 'a pas_vide -> 'b pas_vide

(* J'ai la flemme faire la truc applicative car je ne peut pas creer le checker *)

(** Return pour les pas-vides *)
val return_pas_vide : 'a -> 'a pas_vide

(** Bind pour les pas-vides *)
val ( >>=.|. ) : 'a pas_vide -> ('a -> 'b pas_vide) -> 'b pas_vide
