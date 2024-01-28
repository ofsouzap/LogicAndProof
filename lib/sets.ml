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

let list_of_set (xs : 'a set) : 'a list = xs

let set_of_list (xs : 'a list) : 'a set =
  List.fold_left (fun acc x -> ajoutez x acc) vide xs

let rec membre x = function
  | [] -> false
  | h::ts ->
    if x = h
    then true
    else membre x ts

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
