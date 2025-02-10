(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Logic

let log () =
  let values = LugSet.add 2 (LugSet.add 1 (LugSet.singleton 0)) in
  let truths = LugSet.singleton (LugSet.max_elt values) in
  let and_op =
    { name = "And"
    ; arity = 2
    ; semantic = (fun li -> min (List.nth li 0) (List.nth li 1))
    }
  in
  let or_op =
    { name = "Or"
    ; arity = 2
    ; semantic = (fun li -> max (List.nth li 0) (List.nth li 1))
    }
  in
  let not_op =
    { name = "Not"
    ; arity = 1
    ; semantic = (fun li -> LugSet.max_elt values - List.hd li)
    }
  in
  let imp_op =
    { name = "Imp"
    ; arity = 2
    ; semantic =
        (fun li ->
          min
            (LugSet.max_elt values)
            (LugSet.max_elt values - List.nth li 0 + List.nth li 1))
    }
  in
  let forall = { name = "Forall"; semantic = qt_induce and_op } in
  let exists = { name = "Exists"; semantic = qt_induce or_op } in
  { name = "Lukasiewicz"
  ; values
  ; truths
  ; ops = [ or_op; and_op; not_op; imp_op ]
  ; qts = [ exists; forall ]
  }
