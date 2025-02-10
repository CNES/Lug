(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

let pow base p =
  let rec go acc i =
    match i with
    | 0 -> acc
    | _ -> go (acc * base) (i - 1)
  in
  go 1 p

let show_array arr =
  Array.fold_right (fun x str -> string_of_int x ^ "," ^ str) arr ""
