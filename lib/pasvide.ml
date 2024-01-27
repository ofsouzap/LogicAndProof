type 'a pas_vide =
  | Cons of 'a * 'a pas_vide
  | Feui of 'a

let singleton (x : 'a) : 'a pas_vide = Feui x

let rec ajoutez (x : 'a) (xs : 'a pas_vide) : 'a pas_vide = match xs with
  | Cons (h, ts) -> Cons (h, ajoutez x ts)
  | Feui h -> Cons (h, Feui (x))

let tete (xs : 'a pas_vide) : 'a = match xs with
  | Feui x -> x
  | Cons (h, _) -> h

let tail (xs : 'a pas_vide) : 'a option = match xs with
  | Feui _ -> None
  | Cons (h, _) -> Some h

let reverse (xs : 'a pas_vide) : 'a pas_vide =
  let rec aux (acc : 'a pas_vide) = function
    | Feui x -> Cons (x, acc)
    | Cons (h, ts) -> aux (Cons (h, acc)) ts
  in
  match xs with
    | Feui _ -> xs
    | Cons (h, ts) -> aux (Feui h) ts

let pas_vide_of_list (xs : 'a list) : 'a pas_vide =
  let rec aux (acc : 'a pas_vide) = function
    | [] -> acc
    | h::ts -> aux (Cons (h, acc)) ts
  in
  match xs with
    | [] -> failwith "Liste est vide"
    | h::ts -> aux (Feui h) ts

let map_rev (f : 'a -> 'b) (xs : 'a pas_vide) : 'b pas_vide =
  let rec aux (acc : 'b pas_vide) = function
    | Feui x -> Cons (f x, acc)
    | Cons (h, ts) -> aux (Cons (f h, acc)) ts
  in
  match xs with
    | Feui x -> Feui (f x)
    | Cons (h, ts) -> aux (Feui (f h)) ts

let map (f : 'a -> 'b) (xs : 'a pas_vide) : 'b pas_vide = reverse (map_rev f xs)

let zip_rev (xs : 'a pas_vide) (ys : 'b pas_vide) : ('a * 'b) pas_vide =
  let rec aux (acc : ('a * 'b) pas_vide) = function
    | Feui x, Feui y -> Cons ((x, y), acc)
    | Feui x, Cons (y, _) -> Cons ((x, y), acc)
    | Cons (x, _), Feui y -> Cons ((x, y), acc)
    | Cons (xh, xts), Cons (yh, yts) -> aux (Cons ((xh, yh), acc)) (xts, yts)
  in
  match (xs, ys) with
    | Feui x, Feui y -> Feui (x, y)
    | Feui x, Cons (y, _) -> Feui ((x, y))
    | Cons (x, _), Feui y -> Feui ((x, y))
    | Cons (xh, xts), Cons (yh, yts) -> aux (Feui (xh, yh)) (xts, yts)

let zip (xs : 'a pas_vide) (ys : 'b pas_vide) : ('a * 'b) pas_vide = reverse (zip_rev xs ys)

let rec tous (f : 'a -> bool) (xs : 'a pas_vide) : bool = match xs with
  | Feui x -> f x
  | Cons (h, ts) -> f h && tous f ts

let rec quelque (f : 'a -> bool) (xs : 'a pas_vide) : bool = match xs with
  | Feui x -> f x
  | Cons (h, ts) -> f h || quelque f ts
