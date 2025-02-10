(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Logic

let log () =
  let values = LugSet.add 1 (LugSet.singleton 0) in
  let truths = LugSet.singleton 1 in
  let and_op =
    { name = "And"
    ; arity = 2
    ; semantic =
        (fun li ->
          let u = List.hd li in
          let v = List.nth li 1 in
          match (u, v) with
          | 1, 1 -> 1
          | _ -> 0)
    }
  in
  let or_op =
    { name = "Or"
    ; arity = 2
    ; semantic =
        (fun li ->
          let u = List.nth li 0 in
          let v = List.nth li 1 in
          match (u, v) with
          | 0, 0 -> 0
          | _ -> 1)
    }
  in
  let not_op =
    { name = "Not"
    ; arity = 1
    ; semantic =
        (fun li ->
          let el = List.hd li in
          (el + 1) mod 2)
    }
  in
  let imp_op =
    { name = "Imp"
    ; arity = 2
    ; semantic =
        (fun li ->
          or_op.semantic [ not_op.semantic [ List.nth li 0 ]; List.nth li 1 ])
    }
  in
  let forall = { name = "Forall"; semantic = qt_induce and_op } in
  let exists = { name = "Exists"; semantic = qt_induce or_op } in
  { name = "CPC"
  ; values
  ; truths
  ; ops = [ or_op; and_op; not_op; imp_op ]
  ; qts = [ exists; forall ]
  }
