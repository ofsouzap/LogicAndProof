type 'a set

val formateur_int_set : Format.formatter -> int set -> unit

val vide : 'a set

val singleton : 'a -> 'a set

val ajoutez : 'a -> 'a set -> 'a set

val list_of_set : 'a set -> 'a list

val set_of_list : 'a list -> 'a set

val membre : 'a -> 'a set -> bool

val pareil : 'a set -> 'a set -> bool
