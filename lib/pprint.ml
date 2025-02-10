(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Calculus
open Formula

let show_sequent seq =
  let len = Sequent.cardinal seq in
  let _, res = Sequent.fold
    (fun _ v (i, str) ->
      let li =
        if List.length v = 0 then "[]"
        else List.fold_right (fun fm str -> show_fm fm ^ "," ^ str) v "" ^ "\b"
      in
      let str' = if i = len then str ^ li else str ^ li ^ " => " in
      i + 1, str')
    seq
    (1, "")
    in res

let show_rule rule =
  let _, hyp_str =
    List.fold_right
      (fun h (i, str) ->
        (i + 1, Printf.sprintf "%s\n%d: %s" str i (show_sequent h)))
      rule.hyp
      (1, "")
  in
  let csq_str = show_sequent rule.csq in
  Printf.sprintf "%s\n======================\n%s" hyp_str csq_str
