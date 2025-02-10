(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Formula

type theorem = {
  name: string;
  stmt: formula;
  proof: string list
}
[@@deriving show]

type program = {
  thms : theorem list
}