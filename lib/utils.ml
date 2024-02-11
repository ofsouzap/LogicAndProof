let prennez_rev n xs =
  let rec aux acc n xs = if n < 0 then failwith "Nombre d'elements negatif" else
    match (n, xs) with
      | (_, []) -> acc
      | (n, h::ts) -> aux (h::acc) n ts
  in
  aux [] n xs

let prennez n xs = List.rev (prennez_rev n xs)

let rec intercalez_str sep = function
  | [] -> ""
  | h::[] -> h
  | h::(_::_ as ts) -> h ^ sep ^ intercalez_str sep ts
