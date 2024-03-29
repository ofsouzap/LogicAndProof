open Nicelib

type verite = bool
let vrai : verite = true
let faux : verite = false
let pas = not

type varnom = string
type atome =
  | Lit of verite
  | Var of varnom

let atome_print = function
  | Lit b -> "Lit " ^ string_of_bool b
  | Var nom -> "Var " ^ nom

let atome_arbitraire : atome QCheck.arbitrary = QCheck.make ~print:atome_print (QCheck.Gen.(frequency
  [ 1, map (fun x -> Lit x) bool
  ; 2, map (fun x -> Var x) string_printable
  ]))

type proposition =
  | Atome of atome
  | Ou of proposition Pasvide.pas_vide
  | Et of proposition Pasvide.pas_vide
  | Pas of proposition
  | Impl of proposition * proposition
  | BiImpl of proposition * proposition

let proposition_gen : proposition QCheck.Gen.t = QCheck.Gen.(sized @@ fix
  ( fun self n -> if n < 0 then failwith "n ne peut pas etre negatif" else
    if n < 1 then map (fun x -> Atome x) (QCheck.gen atome_arbitraire)
    else
      let n' = min n 10 in (* Sinon ca serait trop grand *)
      let m = min ((QCheck.Gen.generate1 (int_bound 5)) + 1) n' in
      frequency
      [ 1, map (fun x -> Atome x) (QCheck.gen atome_arbitraire)
      ; 3, map (fun xs -> Ou xs) (QCheck.gen (Pasvide.pas_vide_arbitraire_n m (QCheck.make (self (n'/m)))))
      ; 3, map (fun xs -> Et xs) (QCheck.gen (Pasvide.pas_vide_arbitraire_n m (QCheck.make (self (n'/m)))))
      ; 2, map (fun x -> Pas x) (self (n'-1))
      ; 3, map2 (fun x y -> Impl (x, y)) (self (n'/2)) (self (n'/2))
      ; 3, map2 (fun x y -> BiImpl (x, y)) (self (n'/2)) (self (n'/2))
      ]
  ))

let proposition_arbitraire : proposition QCheck.arbitrary =
  QCheck.make proposition_gen

let rec string_of_proposition = function
  | Atome (Lit b) -> string_of_bool b
  | Atome (Var nom) -> nom
  | Ou es -> "(" ^ Pasvide.intercalez ")+(" (Pasvide.map string_of_proposition es) ^ ")"
  | Et es -> "(" ^ Pasvide.intercalez ").(" (Pasvide.map string_of_proposition es) ^ ")"
  | Pas e -> "¬(" ^ string_of_proposition e ^ ")"
  | Impl (a,b) -> "(" ^ string_of_proposition a ^ ") => (" ^ string_of_proposition b ^ ")"
  | BiImpl (a,b) -> "(" ^ string_of_proposition a ^ ") <=> (" ^ string_of_proposition b ^ ")"

let prop_var_libres (p : proposition) : varnom Sets.t =
  let rec aux acc = function
    | Atome (Lit _) -> acc
    | Atome (Var n) -> Sets.add n acc
    | Ou xs -> Pasvide.foldl aux acc xs
    | Et xs -> Pasvide.foldl aux acc xs
    | Pas x -> aux acc x
    | Impl (a, b) -> aux (aux acc a) b
    | BiImpl (a, b) -> aux (aux acc a) b
  in
  aux Sets.empty p

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

let evaluez_atome (i : interpretation) (a : atome) : verite = match a with
  | Lit b -> b
  | Var n -> (match interpretation_cherche n i with | None -> false | Some b -> b)

type proposition_simple =
  | Atome of atome
  | Ou of proposition_simple Pasvide.pas_vide
  | Et of proposition_simple Pasvide.pas_vide
  | Pas of proposition_simple

let rec string_of_simple = function
  | Atome (Lit b) -> string_of_bool b
  | Atome (Var nom) -> nom
  | Ou es -> "(" ^ Pasvide.intercalez ")+(" (Pasvide.map string_of_simple es) ^ ")"
  | Et es -> "(" ^ Pasvide.intercalez ").(" (Pasvide.map string_of_simple es) ^ ")"
  | Pas e -> "¬(" ^ string_of_simple e ^ ")"

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

type neg_atome =
  | AtomeLit of verite
  | AtomeVar of varnom
  | PasAtomeVar of varnom

let string_of_neg_atome = function
  | AtomeLit b -> string_of_bool b
  | AtomeVar nom -> nom
  | PasAtomeVar nom -> "¬" ^ nom

let neg_atome_print = function
  | AtomeLit b -> "AtomeLit " ^ string_of_bool b
  | AtomeVar nom -> "AtomeVar " ^ nom
  | PasAtomeVar nom -> "PasAtomeVar " ^ nom

let neg_atome_arbitraire : neg_atome QCheck.arbitrary = QCheck.make ~print:neg_atome_print (QCheck.Gen.(frequency
  [ 1, map (fun x -> AtomeLit x) QCheck.Gen.bool
  ; 1, map (fun x -> AtomeVar x) QCheck.Gen.string_printable
  ; 1, map (fun x -> PasAtomeVar x) QCheck.Gen.string_printable
  ]))

type proposition_nnf =
  | Atome of neg_atome
  | Ou of proposition_nnf Pasvide.pas_vide
  | Et of proposition_nnf Pasvide.pas_vide

let nnf_gen : proposition_nnf QCheck.Gen.t = QCheck.Gen.(sized @@ fix
  ( fun self n -> if n < 0 then failwith "n ne peut pas etre negatif" else
    if n < 2 then map (fun x -> Atome x) (QCheck.gen neg_atome_arbitraire)
    else
      let n' = min n 10 in (* Sinon ca serait trop grand *)
      let m = min ((QCheck.Gen.generate1 (int_bound 5)) + 1) n' in
      frequency
      [ 2, map (fun x -> Atome x) (QCheck.gen neg_atome_arbitraire)
      ; 3, map (fun xs -> Ou xs) (QCheck.gen (Pasvide.pas_vide_arbitraire_n m (QCheck.make (self (n'/m)))))
      ; 3, map (fun xs -> Et xs) (QCheck.gen (Pasvide.pas_vide_arbitraire_n m (QCheck.make (self (n'/m)))))
      ]
  ))

let nnf_arbitraire : proposition_nnf QCheck.arbitrary =
  QCheck.make nnf_gen

let rec string_of_nnf = function
  | Atome neg_atome -> string_of_neg_atome neg_atome
  | Ou es -> "(" ^ Pasvide.intercalez ")+(" (Pasvide.map string_of_nnf es) ^ ")"
  | Et es -> "(" ^ Pasvide.intercalez ").(" (Pasvide.map string_of_nnf es) ^ ")"

let rec simple_au_nnf (p : proposition_simple) : proposition_nnf = match p with
  | Atome (Lit b) -> Atome (AtomeLit b)
  | Atome (Var nom) -> Atome (PasAtomeVar nom)
  | Ou xs -> Ou (Pasvide.map simple_au_nnf xs)
  | Et xs -> Et (Pasvide.map simple_au_nnf xs)
  | Pas (Ou xs) ->
    let mapf x : proposition_nnf = simple_au_nnf (Pas x) in
    Et (Pasvide.map mapf xs)
  | Pas (Et xs) ->
    let mapf x : proposition_nnf = simple_au_nnf (Pas x) in
    Ou (Pasvide.map mapf xs)
  | Pas (Atome (Lit b)) -> Atome (AtomeLit (pas b))
  | Pas (Atome (Var nom)) -> Atome (PasAtomeVar nom)
  | Pas (Pas x) -> simple_au_nnf x

let evaluez_neg_atome (i : interpretation) (a : neg_atome) : verite = match a with
  | AtomeLit b -> b
  | AtomeVar nom -> ( match interpretation_cherche nom i with | None -> false | Some b -> b )
  | PasAtomeVar nom -> pas ( match interpretation_cherche nom i with | None -> false | Some b -> b )

let nnf_var_libres (p : proposition_nnf) : varnom Sets.t =
  let rec aux acc = function
    | Atome (AtomeLit _) -> acc
    | Atome (AtomeVar nom) -> Sets.add nom acc
    | Atome (PasAtomeVar nom) -> Sets.add nom acc
    | Ou xs -> Pasvide.foldl aux acc xs
    | Et xs -> Pasvide.foldl aux acc xs
  in
  aux Sets.empty p

let rec evaluez_nnf (i : interpretation) (p : proposition_nnf) : verite = match p with
  | Atome a -> evaluez_neg_atome i a
  | Ou xs -> Pasvide.quelque (evaluez_nnf i) xs
  | Et xs -> Pasvide.tous (evaluez_nnf i) xs

type terme_dnf = neg_atome Pasvide.pas_vide

type proposition_dnf = terme_dnf Pasvide.pas_vide

type terme_cnf = neg_atome Pasvide.pas_vide

type proposition_cnf = terme_dnf Pasvide.pas_vide

let terme_dnf_arbitraire = Pasvide.pas_vide_arbitraire neg_atome_arbitraire

let dnf_arbitraire = Pasvide.pas_vide_arbitraire terme_dnf_arbitraire

let string_of_terme_dnf atomes = "(" ^ (Pasvide.intercalez ").(" (Pasvide.map string_of_neg_atome atomes)) ^ ")"

let string_of_dnf es = "(" ^ (Pasvide.intercalez ")+(" (Pasvide.map string_of_terme_dnf es)) ^ ")"

let terme_cnf_arbitraire = Pasvide.pas_vide_arbitraire neg_atome_arbitraire

let cnf_arbitraire = Pasvide.pas_vide_arbitraire terme_cnf_arbitraire

let string_of_terme_cnf atomes = "(" ^ (Pasvide.intercalez ")+(" (Pasvide.map string_of_neg_atome atomes)) ^ ")"

let string_of_cnf es = "(" ^ (Pasvide.intercalez ").(" (Pasvide.map string_of_terme_cnf es)) ^ ")"

let fussionnez_cartesian_deux (x : 'a Pasvide.pas_vide) (y : 'a Pasvide.pas_vide) : 'a Pasvide.pas_vide =
  let zs = Pasvide.prod_cartesian x y in
  Pasvide.map_rev
    (fun (a,b) -> Pasvide.enchainez a b)
    zs

let fusionnez_cartesian_plusieurs (xs : 'a Pasvide.pas_vide Pasvide.pas_vide) : 'a Pasvide.pas_vide = match xs with
  | Feui x -> x
  | Cons (xh,xts) -> Pasvide.foldl fussionnez_cartesian_deux xh xts

let rec nnf_au_dnf (p : proposition_nnf) : proposition_dnf = match p with
  | Atome a -> Pasvide.singleton (Pasvide.singleton a)
  | Ou xs ->
    let xs' = Pasvide.map nnf_au_dnf xs in
    Pasvide.aplatissez xs'
  | Et xs ->
    let xs' = Pasvide.map nnf_au_dnf xs in
    fusionnez_cartesian_plusieurs xs'

let dnf_var_libres (p : proposition_dnf) : varnom Sets.t =
  let aux (acc : varnom Sets.t) (a : neg_atome) : varnom Sets.t = match a with
    | AtomeLit _ -> acc
    | AtomeVar nom -> Sets.add nom acc
    | PasAtomeVar nom -> Sets.add nom acc
  in
  Pasvide.foldl (Pasvide.foldl aux) Sets.empty p

let evaluez_terme_dnf (i : interpretation) = Pasvide.tous (evaluez_neg_atome i)

let evaluez_dnf (i : interpretation) = Pasvide.quelque (evaluez_terme_dnf i)

let rec nnf_au_cnf (p : proposition_nnf) : proposition_cnf = match p with
  | Atome a -> Pasvide.singleton (Pasvide.singleton a)
  | Ou xs ->
    let xs' = Pasvide.map nnf_au_cnf xs in
    fusionnez_cartesian_plusieurs xs'
  | Et xs ->
    let xs' = Pasvide.map nnf_au_cnf xs in
    Pasvide.aplatissez xs'

let cnf_var_libres (p : proposition_cnf) : varnom Sets.t =
  let aux (acc : varnom Sets.t) (a : neg_atome) : varnom Sets.t = match a with
    | AtomeLit _ -> acc
    | AtomeVar nom -> Sets.add nom acc
    | PasAtomeVar nom -> Sets.add nom acc
  in
  Pasvide.foldl (Pasvide.foldl aux) Sets.empty p

let evaluez_terme_cnf (i : interpretation) = Pasvide.quelque (evaluez_neg_atome i)

let evaluez_cnf (i : interpretation) = Pasvide.tous (evaluez_terme_cnf i)
