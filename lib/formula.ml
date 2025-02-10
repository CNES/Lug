(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

(* A term is either a constant or a function in the given theory. *)

type term =
  | Cst of string
  | Fn of string * term list
[@@deriving show, eq, ord]

module TermSet = Set.Make (struct
  type t = term

  let compare = compare
end)

(* An atom is either a predicate or a proposition. *)

type atom = string * term list [@@deriving show, ord]

module AtomSet = Set.Make (struct
  type t = atom

  let compare = compare
end)

(* General inductive definition of the formula for many-valued logics *)

type formula =
  | Val of int
  | Atm of atom
  | Op of string * formula list
  | Qt of string * string * formula
[@@deriving show, ord]

let show_tuple fn tu =
  if List.length tu = 0 then ""
  else List.fold_left (fun str tm -> str ^ fn tm ^ ",") "(" tu ^ "\b)"

let rec show_fm fm =
  match fm with
  | Val i -> string_of_int i
  | Atm (nm, args) -> nm ^ show_tuple show_term args
  | Op (nm, args) -> nm ^ show_tuple show_fm args
  | Qt (nm, var, fm) -> Printf.sprintf "%s[%s].%s" nm var (show_fm fm)
