type verite = bool
let vrai : verite = true
let faux : verite = false

type varnom = string
type atome =
  | Lit of verite
  | Var of varnom

let atome_gen : atome QCheck.Gen.t = QCheck.Gen.(frequency
  [ 1, map (fun x -> Lit x) bool
  ; 2, map (fun x -> Var x) string_printable
  ])

type proposition =
  | Atome of atome
  | Ou of proposition Pasvide.pas_vide
  | Et of proposition Pasvide.pas_vide
  | Pas of proposition
  | Impl of proposition * proposition
  | BiImpl of proposition * proposition

type interpretation = (varnom * verite) list

let rec interpretation_cherche (nom : varnom) (i : interpretation) : verite option = match i with
  | [] -> None
  | ((hnom, hval)::ts) -> if hnom = nom
    then Some hval
    else interpretation_cherche nom ts

let interpretation_ajoute (nom : varnom) (b : verite) (i : interpretation) : interpretation =
  (nom, b) :: i

(* TODO - faisez ca mieux *)
let rec interpretations_possibles (vars : varnom list) : interpretation list = match vars with
  | [] -> []
  | h::ts ->
    let sub = interpretations_possibles ts in
    List.concat_map
      ( fun i ->
        [ interpretation_ajoute h vrai i
        ; interpretation_ajoute h faux i
        ] )
      sub

let propositions_equivalents vars eval1 eval2 p1 p2 =
  List.for_all
    (fun i -> eval1 i p1 = eval2 i p2)
    (interpretations_possibles (Sets.list_of_set vars))

let prop_var_libres (p : proposition) : varnom Sets.set =
  let rec aux acc = function
    | Atome (Lit _) -> acc
    | Atome (Var n) -> Sets.ajoutez n acc
    | Ou xs -> Pasvide.foldl aux acc xs
    | Et xs -> Pasvide.foldl aux acc xs
    | Pas x -> aux acc x
    | Impl (a, b) -> aux (aux acc a) b
    | BiImpl (a, b) -> aux (aux acc a) b
  in
  aux Sets.vide p

let evaluez_atome (i : interpretation) (a : atome) : verite = match a with
  | Lit b -> b
  | Var n -> (match interpretation_cherche n i with | None -> false | Some b -> b)

type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple Pasvide.pas_vide
  | Et of proposition_simple Pasvide.pas_vide
  | Pas of proposition_simple

let rec prop_au_simple (p : proposition) : proposition_simple = match p with
  | Atome a -> Atome a
  | Ou xs -> Ou (Pasvide.map prop_au_simple xs)
  | Et xs -> Et (Pasvide.map prop_au_simple xs)
  | Pas x -> Pas (prop_au_simple x)
  | Impl (a, b) ->
    let a' = prop_au_simple a in
    let b' = prop_au_simple b in
      Ou (Pasvide.paire (Pas a') b')
  | BiImpl (a, b) ->
    let a' = prop_au_simple a in
    let b' = prop_au_simple b in
      Ou (Pasvide.paire
        (Et (Pasvide.paire a' b'))
        (Et (Pasvide.paire (Pas a') (Pas b')))
      )

let rec evaluez_simple (i : interpretation) = function
  | Atome a -> evaluez_atome i a
  | Ou xs -> Pasvide.quelque (evaluez_simple i) xs
  | Et xs -> Pasvide.tous (evaluez_simple i) xs
  | Pas x -> not (evaluez_simple i x)

let evaluez (i : interpretation) (p : proposition) : verite =
  let p' = prop_au_simple p in
  evaluez_simple i p'

(* TODO - ne liassez pas les neg-atome etre une literale pas-atome *)
type neg_atome =
  | Atome of atome
  | PasAtome of atome

let neg_atome_gen : neg_atome QCheck.Gen.t = QCheck.Gen.(frequency
  [ 1, map (fun x -> Atome x) atome_gen
  ; 1, map (fun x -> PasAtome x) atome_gen
  ])

type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf Pasvide.pas_vide
  | Et of proposition_nnf Pasvide.pas_vide

let nnf_gen : proposition_nnf QCheck.Gen.t = QCheck.Gen.(sized @@ fix
  ( fun self n -> if n < 0 then failwith "n ne peut pas etre negatif" else match n with
    | 0 -> map (fun x -> Atome x) neg_atome_gen
    | n' ->
      let n = min n' 10 in (* Sinon ca serait trop grand *)
      let m = min ((QCheck.Gen.generate1 (int_bound 5)) + 1) n in
      frequency
      [ 2, map (fun x -> Atome x) neg_atome_gen
      ; 3, map (fun xs -> Ou xs) ((Pasvide.pas_vide_gen_n m) (self (n-m)))
      ; 3, map (fun xs -> Et xs) ((Pasvide.pas_vide_gen_n m) (self (n-m)))
      ]
  ))

let nnf_arbitraire : proposition_nnf QCheck.arbitrary =
  QCheck.make nnf_gen

let rec simple_au_nnf (p : proposition_simple) : proposition_nnf = match p with
  | Atome a -> Atome (Atome a)
  | Ou xs -> Ou (Pasvide.map simple_au_nnf xs)
  | Et xs -> Et (Pasvide.map simple_au_nnf xs)
  | Pas (Ou xs) ->
    let mapf x : proposition_nnf = simple_au_nnf (Pas x) in
    Et (Pasvide.map mapf xs)
  | Pas (Et xs) ->
    let mapf x : proposition_nnf = simple_au_nnf (Pas x) in
    Ou (Pasvide.map mapf xs)
  | Pas (Atome a) -> Atome (PasAtome a)
  | Pas (Pas x) -> simple_au_nnf x

let evaluez_neg_atome (i : interpretation) (a : neg_atome) : verite = match a with
  | Atome a -> evaluez_atome i a
  | PasAtome a -> not (evaluez_atome i a)

let nnf_var_libres (p : proposition_nnf) : varnom Sets.set =
  let rec aux acc = function
    | Atome (Atome (Lit _)) -> acc
    | Atome (PasAtome (Lit _)) -> acc
    | Atome (Atome (Var n)) -> Sets.ajoutez n acc
    | Atome (PasAtome (Var n)) -> Sets.ajoutez n acc
    | Ou xs -> Pasvide.foldl aux acc xs
    | Et xs -> Pasvide.foldl aux acc xs
  in
  aux Sets.vide p

let rec evaluez_nnf (i : interpretation) (p : proposition_nnf) : verite = match p with
  | Atome a -> evaluez_neg_atome i a
  | Ou xs -> Pasvide.quelque (evaluez_nnf i) xs
  | Et xs -> Pasvide.tous (evaluez_nnf i) xs

type terme_dnf = neg_atome Pasvide.pas_vide

type proposition_dnf = terme_dnf Pasvide.pas_vide

let terme_dnf_gen = Pasvide.pas_vide_gen neg_atome_gen

let dnf_gen = Pasvide.pas_vide_gen terme_dnf_gen

let dnf_arbitraire : proposition_dnf QCheck.arbitrary =
  QCheck.make dnf_gen

let fussionnez_deux_dnf (x : proposition_dnf) (y : proposition_dnf) : proposition_dnf =
  let zs = Pasvide.prod_cartesian x y in
  Pasvide.map_rev
    (fun (a,b) -> Pasvide.enchainez a b)
    zs

let fusionnez_dnf (xs : proposition_dnf Pasvide.pas_vide) : proposition_dnf = match xs with
  | Feui x -> x
  | Cons (xh,xts) -> Pasvide.foldl fussionnez_deux_dnf xh xts

let aplatissez_dnf (xs : proposition_dnf Pasvide.pas_vide) : proposition_dnf = match xs with
  | Feui x -> x
  | Cons (xh, xts) -> Pasvide.foldl Pasvide.enchainez xh xts

let rec nnf_au_dnf (p : proposition_nnf) : proposition_dnf = match p with
  | Atome a -> Pasvide.singleton (Pasvide.singleton a)
  | Ou xs ->
    let xs' = Pasvide.map nnf_au_dnf xs in
    aplatissez_dnf xs'
  | Et xs ->
    let xs' = Pasvide.map nnf_au_dnf xs in
    fusionnez_dnf xs'

let dnf_var_libres (p : proposition_dnf) : varnom Sets.set =
  let aux (acc : varnom Sets.set) (a : neg_atome) : varnom Sets.set = match a with
    | Atome (Lit _) -> acc
    | PasAtome (Lit _) -> acc
    | Atome (Var n) -> Sets.ajoutez n acc
    | PasAtome (Var n) -> Sets.ajoutez n acc
  in
  Pasvide.foldl (Pasvide.foldl aux) Sets.vide p

let evaluez_terme_dnf (i : interpretation) = Pasvide.tous (evaluez_neg_atome i)

let evaluez_dnf (i : interpretation) = Pasvide.quelque (evaluez_terme_dnf i)

(** TODO - faisez le truc pour transformer les proposition en CNF *)
