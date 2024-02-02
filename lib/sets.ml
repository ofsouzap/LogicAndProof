(* TODO - faissez un set propre, avec les arbres binares *)

type 'a set = 'a list

let rec formateur_int_set ppf = function
  | [] -> Fmt.pf ppf ""
  | h::ts -> Fmt.pf ppf "%d, %a" h formateur_int_set ts

let rec formateur_string_set ppf = function
  | [] -> Fmt.pf ppf ""
  | h::ts -> Fmt.pf ppf "%s, %a" h formateur_string_set ts

let vide : 'a set = []

let singleton (x : 'a) : 'a set = [x]

let rec ajoutez x = function
  | [] -> [x]
  | h::ts as xs ->
    if x = h
    then xs
    else h :: ajoutez x ts

let rec membre x = function
  | [] -> false
  | h::ts ->
    if x = h
    then true
    else membre x ts

let compte = List.length

let intersection xs =
  let rec aux acc = function
    | [] -> acc
    | h::ts -> aux
      (if membre h xs then ajoutez h acc else acc)
      ts
  in
  aux vide

let rec union xs = function
  | [] -> xs
  | h::ts -> union (ajoutez h xs) ts

let rec pareil (xs : 'a list) (ys : 'a list) : bool =
  let rec essaiez_enlever (x : 'a) (acc : 'a list) = function
    | [] -> None
    | h::ts ->
      if h = x
      then Some (acc @ ts)
      else essaiez_enlever x (acc @ [h]) ts
  in
  match (xs, ys) with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | (xh::xts), ys -> ( match essaiez_enlever xh [] ys with
      | None -> false
      | Some ys' -> pareil xts ys' )

let list_of_set (xs : 'a set) : 'a list = xs

let set_of_list (xs : 'a list) : 'a set =
  List.fold_left (fun acc x -> ajoutez x acc) vide xs

let set_gen gen = QCheck.Gen.map set_of_list QCheck.Gen.(list gen)

let set_gen_max n gen = QCheck.Gen.map set_of_list (QCheck.Gen.map (Utils.prennez_rev n) (QCheck.Gen.(list gen)))

let set_arbitraire_print arb = match QCheck.get_print arb with
  | None -> fun _ -> ""
  | Some p -> QCheck.Print.list p

let set_arbitraire arb = QCheck.make (set_gen (QCheck.gen arb)) ~print:(set_arbitraire_print arb)

let set_arbitraire_max n arb = QCheck.make (set_gen_max n (QCheck.gen arb)) ~print:(set_arbitraire_print arb)
