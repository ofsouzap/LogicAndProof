type 'a pas_vide =
  | Cons of 'a * 'a pas_vide
  | Feui of 'a

let singleton (x : 'a) : 'a pas_vide = Feui x

let paire (a : 'a) (b : 'a) : 'a pas_vide = Cons (a, Feui b)

let ajoutez (x : 'a ) (xs : 'a pas_vide) : 'a pas_vide = Cons (x, xs)

let rec apposez (x : 'a) (xs : 'a pas_vide) : 'a pas_vide = match xs with
  | Cons (h, ts) -> Cons (h, apposez x ts)
  | Feui h -> Cons (h, Feui (x))

let rec enchainez (xs : 'a pas_vide) (ys : 'a pas_vide) : 'a pas_vide = match xs with
  | Feui x -> Cons (x, ys)
  | Cons (xh,xts) -> Cons (xh, enchainez xts ys)

let (@:) = enchainez

let tete (xs : 'a pas_vide) : 'a = match xs with
  | Feui x -> x
  | Cons (h, _) -> h

let tail (xs : 'a pas_vide) : 'a pas_vide option = match xs with
  | Feui _ -> None
  | Cons (_, ts) -> Some ts

let renverse (xs : 'a pas_vide) : 'a pas_vide =
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
    | h::ts -> renverse (aux (Feui h) ts)

let list_of_pas_vide (xs : 'a pas_vide) : 'a list =
  let rec aux (acc : 'a list) = function
    | Feui x -> x :: acc
    | Cons (h, ts) -> aux (h :: acc) ts
  in
  List.rev (aux [] xs)

let rec foldl (f : 'b -> 'a -> 'b) (acc : 'b) (xs : 'a pas_vide) : 'b = match xs with
  | Feui x -> f acc x
  | Cons (h,ts) -> foldl f (f acc h) ts

let rec foldr (f : 'a -> 'b -> 'b) (acc : 'b) (xs : 'a pas_vide) : 'b = match xs with
  | Feui x -> f x acc
  | Cons (h,ts) -> f h (foldr f acc ts)

let map_rev (f : 'a -> 'b) (xs : 'a pas_vide) : 'b pas_vide =
  let foldf (acc : 'b pas_vide) (x : 'a) : 'b pas_vide =
    ajoutez (f x) acc
  in
  match xs with
    | Feui x -> Feui (f x)
    | Cons (h,ts) -> foldl foldf (Feui (f h)) ts

let map (f : 'a -> 'b) (xs : 'a pas_vide) : 'b pas_vide = renverse (map_rev f xs)

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

let zip (xs : 'a pas_vide) (ys : 'b pas_vide) : ('a * 'b) pas_vide = renverse (zip_rev xs ys)

let rec tous (f : 'a -> bool) (xs : 'a pas_vide) : bool = match xs with
  | Feui x -> f x
  | Cons (h, ts) -> f h && tous f ts

let rec quelque (f : 'a -> bool) (xs : 'a pas_vide) : bool = match xs with
  | Feui x -> f x
  | Cons (h, ts) -> f h || quelque f ts

let compte xs = foldl (fun acc _ -> succ acc) 0 xs

let prod_cartesian (xs : 'a pas_vide) (ys : 'b pas_vide) : ('a * 'b) pas_vide =
  let rec aux_sub (x : 'a) (acc : ('a * 'b) pas_vide) = function
    | Feui y -> Cons ((x,y), acc)
    | Cons (yh,yts) -> aux_sub x (Cons ((x,yh), acc)) yts
  in
  let rec aux_principle (acc : ('a * 'b) pas_vide) (ys : 'b pas_vide) = function
    | Feui x -> aux_sub x acc ys
    | Cons (xh,xts) -> aux_principle (aux_sub xh acc ys) ys xts
  in
  match (xs, ys) with
    | (Feui x, Feui y) -> singleton (x, y)
    | (Feui x, Cons (yh,yts)) -> aux_sub x (Feui (x, yh)) yts
    | (Cons (xh,xts), (Feui y as ys)) -> aux_principle (Feui (xh,y)) ys xts
    | (Cons (xh,xts), (Cons (yh,yts) as ys)) -> aux_principle (aux_sub xh (Feui (xh,yh)) yts) ys xts

let pas_vide_arbitraire_print arb = match QCheck.get_print arb with
  | None -> fun _ -> ""
  | Some p -> fun xs -> (QCheck.Print.list p) (list_of_pas_vide xs)

let pas_vide_arbitraire arb = QCheck.make
  ~print:(pas_vide_arbitraire_print arb)
  QCheck.Gen.( map
    ( fun (h,ts) -> pas_vide_of_list (h::ts) )
    ( pair (QCheck.gen arb) (list (QCheck.gen arb)) ) )

let pas_vide_gen_n n gen_ele = QCheck.Gen.(fix
  ( fun self n ->
    if n < 1 then map (fun x -> Feui x) gen_ele
    else map (fun (h,ts) -> Cons (h,ts)) (pair gen_ele (self (n-1))) ))
  n

let pas_vide_arbitraire_n n arb = QCheck.make
  ~print:(pas_vide_arbitraire_print arb)
  (pas_vide_gen_n n (QCheck.gen arb))
