(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Calculus
open Logic
open Syntax
module RulesMap = Map.Make (String)

let build_rules log =
  List.fold_right
    (fun (op: operator) acc ->
      LugSet.fold
        (fun v (acc_li, acc) ->
          let rule = Calculus.gen_op_rule log op v in
          Printf.printf "[i] adding rule %s to the calculus.\n%s\n" rule.name
             (Pprint.show_rule rule);
          (rule :: acc_li, RulesMap.add rule.name rule acc))
        log.values
        acc)
    log.ops
    ([], RulesMap.empty)

let show_success () = "[\x1B[32m⬤\x1B[0m]"

let show_failure () = "[\x1B[31m⬤\x1B[0m]"

let show_goals goals =
  Printf.printf "\t\x1B[37m%d Goal(s)\n" (List.length goals);
  List.iter
    (fun goal -> Printf.printf "\t* %s\n" (Pprint.show_sequent goal))
    goals;
  Printf.printf "\x1B[0m"

let apply_axiom log seq lab t =
  Printf.printf "\tapply axiom. ";
  if Calculus.is_tautology seq log then (
    Printf.printf "%s\n" (show_success ());
    t)
  else (
    Printf.printf "%s\n" (show_failure ());
    lab)

let apply_auto log rules seq lab t =
  Printf.printf "\tapply auto.";
  if Calculus.auto log rules seq then (
    Printf.printf " %s\n" (show_success ());
    t)
  else (
    Printf.printf " %s\n" (show_failure ());
    lab)

(* Executes the proof on each theorem *)

let exec fname =
  let prog = Parse.parse fname in
  let log = Luk.log () in
  let rules_li, rules = build_rules log in
  List.iter
    (fun thm ->
      Printf.printf
        "\nTheorem %s : %s.\nProof.\n"
        thm.name
        (Formula.show_fm thm.stmt);
      let end_seq =
        List.fold_left
          (fun acc rn ->
            match acc with
            | [] -> []
            | hd_seq :: t ->
              let goals =
                match rn with
                | "axiom" -> apply_axiom log hd_seq acc t
                | "auto" -> apply_auto log rules_li hd_seq acc t
                | _ -> (
                  let rule = RulesMap.find rn rules in
                  match Calculus.apply rule hd_seq with
                  | None ->
                    Printf.printf "\tapply %s. %s\n" rule.name (show_failure ());
                    acc
                  | Some s ->
                    Printf.printf "\tapply %s. %s\n" rule.name (show_success ());
                    s @ t)
              in
              show_goals goals;
              goals)
          [ Calculus.Sequent.add
              (LugSet.max_elt log.values)
              [ thm.stmt ]
              Calculus.Sequent.empty
          ]
          thm.proof
      in
      if List.length end_seq = 0 then Printf.printf "\tQed.\n"
      else Printf.printf "\tInvalid proof.\n")
    prog.thms
