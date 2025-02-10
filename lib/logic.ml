(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

module LugSet = Set.Make (Int)

let next_val el set = LugSet.find_first (fun e -> e > el) set

let gen_ls n =
  let rec go acc = function
    | 0 -> acc
    | i -> go (LugSet.add i acc) (i - 1)
  in
  go LugSet.empty n

(* Pretty printer *)

let show_ls set =
  LugSet.fold (fun el str -> str ^ string_of_int el ^ ",") set "[" ^ "]"

module LSSet = Set.Make (struct
  type t = LugSet.t

  let compare = compare
end)

let next x ls = LugSet.find_first (fun y -> x < y) ls

type operator =
  { name : string
  ; arity : int
  ; semantic : LugSet.elt list -> LugSet.elt
  }

type quantifier =
  { name : string
  ; semantic : LugSet.t -> LugSet.elt
  }

type logic =
  { name : string
  ; values : LugSet.t
  ; truths : LugSet.t
  ; ops : operator list
  ; qts : quantifier list
  }

(* Defines the following order relation for the operator op : u <= v iff op (u,
   v) = v *)

let op_ord (op : operator) u v = op.semantic [ u; v ] = v

(* The supremum element depending on the order relation ord *)

let sup ord vset =
  LugSet.fold
    (fun u sp -> if ord sp u then u else sp)
    vset
    (LugSet.min_elt vset)

(* Generates the semantic of the quantifier based on a two-arity logical
   operator *)

let qt_induce qt vset = sup (op_ord qt) vset

(* Computes the set of parts of LugSet *)

let parts_of s =
  let go st =
    LugSet.fold (fun e acc -> LSSet.add (LugSet.add e st) acc) s LSSet.empty
  in
  LugSet.fold
    (fun _ acc -> LSSet.fold (fun e acc -> LSSet.union acc (go e)) acc acc)
    s
    (LSSet.singleton LugSet.empty)