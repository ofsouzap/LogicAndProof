open Utils
open Nicelib
open Propositions

type hypothese = Hypo of proposition
let emballez_hypo x = Hypo x
let deemballez_hypo (Hypo x) = x

type conclusion = Conc of proposition
let emballez_conc x = Conc x
let deemballez_conc (Conc x) = x

type sequent = hypothese Sets.t * conclusion Sets.t

let make (hypos, concs) =
  let open Nicelib.Functor in
  (emballez_hypo <$>~~ hypos, emballez_conc <$>~~ concs)

let make_listes (hypos, concs) =
  make (Sets.set_of_list hypos, Sets.set_of_list concs)

let hypotheses ((sh,_) : sequent) =
  let open Nicelib.Functor in
  deemballez_hypo <$>~~ sh

let conclusions ((_,sc) : sequent) =
  let open Nicelib.Functor in
  deemballez_conc <$>~~ sc

let string_of_sequent ((hs, cs) : sequent) =
  let open Nicelib.Functor in
    "(" ^ intercalez_str "),(" (List.map string_of_proposition (deemballez_hypo <$>.. Sets.list_of_set hs)) ^ ")"
  ^ "⊢"
  ^ "(" ^ intercalez_str "),(" (List.map string_of_proposition (deemballez_conc <$>.. Sets.list_of_set cs)) ^ ")"

let sequent_est_axiome ((sh,sc) : sequent) : bool =
  let rec aux = function
    | [] -> false
    | (Hypo hh)::hts -> Sets.member (Conc hh) sc || aux hts
  in
  aux (Sets.list_of_set sh)

let prec_neg_gauche ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let rec aux = function
    | [] -> Seq.Nil
    | (Hypo hh)::hts ->
      let sh' = Sets.remove (Hypo hh) sh in
      let sc' = Sets.add (Conc (Pas hh : proposition)) sc in
      Seq.Cons (Sets.singleton (sh', sc'), fun () -> aux hts)
  in
  aux (Sets.list_of_set sh)

let prec_neg_droit ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let rec aux = function
    | [] -> Seq.Nil
    | (Conc ch)::cts ->
      let sc' = Sets.remove (Conc ch) sc in
      let sh' = Sets.add (Hypo (Pas ch : proposition)) sh in
      Seq.Cons (Sets.singleton (sh', sc'), fun () -> aux cts)
  in
  aux (Sets.list_of_set sc)

let prec_conj_gauche ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let open Nicelib.Utils in
  let open Nicelib.Monoid in
  let rec aux : hypothese list -> sequent Sets.t Seq.node = function
    | [] -> Seq.Nil
    | (Hypo hh)::hts -> ( match hh with
      | Et es as hh ->
        let sh' = ((Sets.set_of_list -.- List.map emballez_hypo -.- Pasvide.list_of_pas_vide) es) <>~~ (Sets.remove (Hypo hh) sh) in
        Seq.Cons (Sets.singleton (sh', sc), fun () -> aux hts)
      | _ -> aux hts )
  in
  aux (Sets.list_of_set sh)

let prec_conj_droit ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let open Nicelib.Utils in
  let open Nicelib.Monoid in
  let open Nicelib.Functor in
  let creez_pred (sc' : conclusion Sets.t) (e : proposition) : sequent = (sh, sc' <>~~ (Sets.singleton -.- emballez_conc) e) in
  let rec aux : conclusion list -> sequent Sets.t Seq.node = function
    | [] -> Seq.Nil
    | (Conc ch)::cts -> ( match ch with
      | Et es as ch ->
        let sc' = Sets.remove (Conc ch) sc in
        Seq.Cons ( (creez_pred sc' <$>~~ ((Sets.set_of_list -.- Pasvide.list_of_pas_vide) es)), fun () -> aux cts)
      | _ -> aux cts )
  in
  aux (Sets.list_of_set sc)

let prec_disj_gauche ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let open Nicelib.Utils in
  let open Nicelib.Monoid in
  let open Nicelib.Functor in
  let creez_pred (sh' : hypothese Sets.t) (e : proposition) : sequent = (sh' <>~~ (Sets.singleton -.- emballez_hypo) e, sc) in
  let rec aux : hypothese list -> sequent Sets.t Seq.node = function
    | [] -> Seq.Nil
    | (Hypo hh)::hts -> ( match hh with
      | Ou es as hh ->
        let sh' = Sets.remove (Hypo hh) sh in
        Seq.Cons ( (creez_pred sh' <$>~~ ((Sets.set_of_list -.- Pasvide.list_of_pas_vide) es)), fun () -> aux hts)
      | _ -> aux hts )
  in
  aux (Sets.list_of_set sh)

let prec_disj_droit ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let open Nicelib.Utils in
  let open Nicelib.Monoid in
  let rec aux : conclusion list -> sequent Sets.t Seq.node = function
    | [] -> Seq.Nil
    | (Conc ch)::cts -> ( match ch with
      | Ou es as ch ->
        let sc' = ((Sets.set_of_list -.- List.map emballez_conc -.- Pasvide.list_of_pas_vide) es) <>~~ (Sets.remove (Conc ch) sc) in
        Seq.Cons (Sets.singleton (sh, sc'), fun () -> aux cts)
      | _ -> aux cts )
  in
  aux (Sets.list_of_set sc)

let prec_impl_gauche ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let creez_preds (sh' : hypothese Sets.t) (sc' : conclusion Sets.t) (a : proposition) (b : proposition) : sequent Sets.t =
    let s_a = (sh', Sets.add (Conc a) sc') in
    let s_b = (Sets.add (Hypo b) sh', sc') in
    Sets.set_of_list [s_a; s_b]
  in
  let rec aux : hypothese list -> sequent Sets.t Seq.node = function
    | [] -> Seq.Nil
    | (Hypo hh)::hts -> ( match hh with
      | Impl (a,b) as hh ->
        let sh' = Sets.remove (Hypo hh) sh in
        Seq.Cons (creez_preds sh' sc a b, fun () -> aux hts)
      | _ -> aux hts )
  in
  aux (Sets.list_of_set sh)

let prec_impl_droit ((sh,sc) : sequent) () : sequent Sets.t Seq.node =
  let rec aux : conclusion list -> sequent Sets.t Seq.node = function
    | [] -> Seq.Nil
    | (Conc ch)::cts -> ( match ch with
      | Impl (a,b) as ch ->
        let sh' = sh in
        let sc' = Sets.remove (Conc ch) sc in
        Seq.Cons ( Sets.singleton (Sets.add (Hypo a) sh', Sets.add (Conc b) sc'), fun () -> aux cts )
      | _ -> aux cts )
    in
    aux (Sets.list_of_set sc)
