(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Logic
open Formula
open Utils

(* A sequent is an object of the form A_1 | ... | A_n Where the A_i are list of
   formulae i.e. disjunction *)

module Sequent = Map.Make (Int)

type rule =
  { name : string
  ; hyp : formula list Sequent.t list
  ; csq : formula list Sequent.t
  }

let fmt_var i = "?A" ^ string_of_int i

let fmt_cxt i = "?G" ^ string_of_int i

let fmt_rule nm i = nm ^ "_" ^ string_of_int i

(* Sequents utils *)

let fill_sequent w seq =
  LugSet.fold (fun v seq -> Sequent.add v [ Atm (fmt_cxt v, []) ] seq) w seq

let sequent_update seq k fm =
  Sequent.update
    k
    (fun opt ->
      match opt with
      | None -> Some [ fm ]
      | Some z -> Some (fm :: z))
    seq

let is_tautology seq log =
  if Sequent.cardinal seq <> LugSet.cardinal log.values then false
  else
    let rec go acc = function
      | [] -> acc
      | var :: t ->
        go (acc || Sequent.for_all (fun _ v -> List.mem var v) seq) t
    in
    go false (Sequent.fold (fun _ v li -> li @ v) seq [])

(* Operators' generation for a given operator to a given value *)

(* Builds the next uplet as an array *)
let next arr log =
  let rec go = function
    | 0 -> arr
    | i ->
      if arr.(i - 1) = LugSet.max_elt log.values then (
        arr.(i - 1) <- LugSet.min_elt log.values;
        go (i - 1))
      else (
        arr.(i - 1) <- next_val arr.(i - 1) log.values;
        arr)
  in
  go (Array.length arr)

(* Builds a list of every array in W^n *)

let get_arrays n log flt =
  let rec go curr acc = function
    | 0 -> acc
    | i ->
      let arr = next curr log in
      if flt arr then go arr (Array.copy arr :: acc) (i - 1)
      else go arr acc (i - 1)
  in
  go
    (Array.make n (LugSet.min_elt log.values))
    []
    (pow (LugSet.cardinal log.values) n)

(* Generates the rule for the operator op at truth value v *)

type node = int * LugSet.elt

module NodeSet = Set.Make (struct
  type t = node

  let compare = compare
end)

module Graph = Map.Make (struct
  type t = node

  let compare = compare
end)

module LabelMap = Map.Make (struct
  type t = node

  let compare = compare
end)

(* According to the n-uplet arr, it builds the graph i.e. an edge between each
   arr.(i) *)

let edge_update node_a node_b gr =
  Graph.update
    node_a
    (fun opt ->
      match opt with
      | None -> Some (NodeSet.singleton node_b)
      | Some z -> Some (NodeSet.add node_b z))
    gr

let edge_folding f lim arr _ (i, gr) =
  if i = lim then (f i, Graph.add (f i, arr.(f i)) NodeSet.empty gr)
  else
    let node_a = (f i, arr.(f i)) in
    let node_b = (i, arr.(i)) in
    (f i, edge_update node_a node_b gr)

let gen_edges arr graph sym =
  let ef =
    if not sym then edge_folding (fun x -> x - 1) (Array.length arr) arr
    else edge_folding (fun x -> x + 1) (-1) arr
  in
  let bg = if not sym then Array.length arr else -1 in
  let _, res = Array.fold_right ef arr (bg, graph) in
  res

(* Generates the graph for a given semantics *)

let gen_graph log op v sym =
  let uplets =
    get_arrays op.arity log (fun arr -> op.semantic (Array.to_list arr) = v)
  in
  List.fold_right (fun u gr -> gen_edges u gr sym) uplets Graph.empty

(* Retrieves every node of the given graph *)

let nodes gr = Graph.fold (fun nd _ acc -> NodeSet.add nd acc) gr NodeSet.empty

(* Returns the complementary graph *)

let revert graph log op =
  let nodes = nodes graph in
  Graph.fold
    (fun nd ns acc ->
      let arg_k, _ = nd in
      if arg_k = op.arity - 1 then Graph.add nd NodeSet.empty acc
      else
        let nds =
          LugSet.fold
            (fun v acc -> NodeSet.add (arg_k + 1, v) acc)
            log.values
            NodeSet.empty
        in
        let dt = NodeSet.inter nodes (NodeSet.diff nds ns) in
        if NodeSet.is_empty dt then acc else Graph.add nd dt acc)
    graph
    Graph.empty

(* Find every path of the graph with respect to the layer i.e. only going
   forward *)

let paths graph =
  let rec go sck m p =
    NodeSet.fold
      (fun node acc ->
        let z = Graph.find node graph in
        match NodeSet.cardinal z with
        | 0 when not (NodeSet.is_empty p) -> NodeSet.add node p :: acc
        | 0 when NodeSet.is_empty p -> acc
        | _ -> go z acc (NodeSet.add node p))
      sck
      m
  in
  let layer =
    Graph.fold
      (fun k _ acc ->
        let i, _ = k in
        if i = 0 then NodeSet.add k acc else acc)
      graph
      NodeSet.empty
  in
  go layer [] NodeSet.empty

(* Pretty printer *)

let show_node (l, v) = Printf.sprintf "(%d,%d)" l v

let show_ns ns =
  NodeSet.fold (fun nd str -> str ^ show_node nd ^ ",") ns "[" ^ "]"

let show_graph gr =
  Graph.fold
    (fun k v str ->
      let l, w = k in
      str
      ^ Printf.sprintf
          "(%d, %d) : %d -> %s\n"
          l
          w
          (NodeSet.cardinal v)
          (show_ns v))
    gr
    ""

let show_label_map lm =
  LabelMap.fold
    (fun k v str -> str ^ Printf.sprintf "%s -> %s\n" (show_node k) (show_ls v))
    lm
    ""

let get_vals nb lab =
  NodeSet.fold
    (fun node set ->
      let opt = LabelMap.find_opt node lab in
      match opt with
      | None -> set
      | Some x -> LugSet.union set x)
    nb
    LugSet.empty

(* Generates the rule for the operator op to the value v *)

let update_lab node ls lab =
  LabelMap.update
    node
    (fun opt ->
      match opt with
      | None -> Some ls
      | Some s -> Some (LugSet.union s ls))
    lab

let assign log op v =
  let gr = gen_graph log op v false in
  let sym_gr = gen_graph log op v true in
  let rg = revert gr log op in
  let gr_paths = paths gr in
  let rg_paths = paths rg in
  let seq_set = gen_ls (op.arity + 1) in
  let rec go path third lab = function
    | [] -> (third, lab)
    | p :: t ->
      let com = NodeSet.inter path p in
      let set = gen_ls (NodeSet.cardinal com) in
      let third, _, lab =
        NodeSet.fold
          (fun node (third, set, acc) ->
            if LabelMap.mem node lab then (third, set, acc)
            else
              let neighbours =
                NodeSet.union (Graph.find node gr) (Graph.find node sym_gr)
              in
              let vals = get_vals neighbours acc in
              let third, c, acc =
                if LugSet.cardinal vals > 0 then (
                  (* If vals isn't empty, then there is a cycle and we need a
                     third sequent to avoid the wrong cases *)
                  (* let ss = LugSet.add (LugSet.max_elt seq_set + 1) seq_set in *)
                  let sing = LugSet.singleton (LugSet.max_elt seq_set) in
                  (* Updates the neighbours' assignement *)
                  let lab =
                    NodeSet.fold
                      (fun n acc -> update_lab n sing acc)
                      neighbours
                      lab
                  in
                  (true, LugSet.choose (LugSet.diff seq_set vals), lab))
                else (third, LugSet.choose set, lab)
              in
              let e = LugSet.singleton c in
              (third, LugSet.diff set e, update_lab node e acc))
          com
          (third, set, lab)
      in
      go path third lab t
  in
  let third, lab =
    List.fold_right
      (fun path (third, acc) -> go path third acc gr_paths)
      rg_paths
      (false, LabelMap.empty)
  in
  let seq_set = if third then seq_set else gen_ls op.arity in
  (* Gets the remaining nodes to be labelized *)
  let gr_union path =
    List.fold_right (fun ns acc -> NodeSet.union ns acc) path NodeSet.empty
  in
  (* let nodes_gr = NodeSet.union (nodes gr) (gr_union gr_paths) in *)
  let nodes_rg = gr_union rg_paths in
  let remaining = NodeSet.diff (nodes gr) nodes_rg in
  let rec og acc rem =
    match NodeSet.cardinal rem with
    | 0 -> acc
    | _ ->
      let node = NodeSet.max_elt rem in
      let neighbours =
        NodeSet.union (Graph.find node gr) (Graph.find node sym_gr)
      in
      let removed = NodeSet.remove node rem in
      if NodeSet.cardinal neighbours = 0 then
        og
          (update_lab node (LugSet.singleton (LugSet.choose seq_set)) acc)
          removed
      else
        let acc' =
          NodeSet.fold
            (fun nb acc ->
              match LabelMap.find_opt nb acc with
              | None when not (LabelMap.mem node acc) ->
                LabelMap.add node (LugSet.singleton (LugSet.choose seq_set)) acc
              | None -> acc
              | Some z ->
                let dt = LugSet.diff seq_set z in
                update_lab node dt acc)
            neighbours
            acc
        in
        (* let vals = get_vals neighbours acc in let delta = LugSet.diff seq_set
           vals in Printf.printf "%s -> %s -- %s\n" (show_node node) (show_ls
           delta) (show_ls vals); let deg = match LugSet.cardinal delta with | 0
           -> vals | _ -> delta in *)
        og acc' removed
  in
  let res = og lab remaining in
  (LugSet.cardinal seq_set, res)

(* Generates the operator rule at value v *)

let gen_op_rule log (op : operator) v =
  let ss_len, assignation = assign log op v in
  (* Builds the sequent from the previous assignation *)
  let rec init seqs = function
    | 0 -> seqs
    | i ->
      let fll =
        LabelMap.fold
          (fun (l, w) v seq ->
            if LugSet.mem i v then sequent_update seq w (Atm (fmt_var l, []))
            else seq)
          assignation
          (fill_sequent log.values Sequent.empty)
      in
      if is_tautology fll log || List.mem fll seqs then init seqs (i - 1)
      else init (fll :: seqs) (i - 1)
  in
  let hyp = init [] ss_len in
  let rec args acc = function
    | 0 -> acc
    | i -> args (Atm (fmt_var (i - 1), []) :: acc) (i - 1)
  in
  let csq =
    sequent_update
      (fill_sequent log.values Sequent.empty)
      v
      (Op (op.name, args [] op.arity))
  in
  { name = fmt_rule op.name v; hyp; csq }

(* Generates the rule for the quantifier qt at truth value v *)

let gen_qt_rule log qt v =
  let parts_of_vls = parts_of log.values in
  let flt =
    LSSet.filter
      (fun s ->
        if LugSet.equal s LugSet.empty then false else qt.semantic s = v)
      parts_of_vls
  in
  let fma = fmt_var 0 in
  let hyp =
    match LSSet.cardinal flt with
    | 0 -> [ Sequent.empty ]
    (* If there is only one element, then we can just assign a constant symbol
       that is fresh with respect to the entire system. *)
    | 1 -> [ Sequent.singleton v [ Atm (fma, [ Cst "z" ]) ] ]
    (* If there is more than one element, then we need to introduce a new
       function symbol that takes as arguments the free variables of the whole
       clause. *)
    | _ -> [ Sequent.singleton v [ Atm (fma, [ Fn ("f", [ Cst "y" ]) ]) ] ]
  in
  { name = fmt_rule qt.name v
  ; hyp
  ; csq = Sequent.singleton v [ Qt (qt.name, "x", Atm (fma, [ Cst "x" ])) ]
  }

(* Structural rules' functions *)

let weak k log =
  let hyp = [ fill_sequent log.values Sequent.empty ] in
  let csq = fill_sequent log.values Sequent.empty in
  let elm = Atm ("A", []) :: Sequent.find k csq in
  let csq = Sequent.add k elm csq in
  { name = fmt_rule "weak" k; hyp; csq }

(* Rules application to formulae *)

module FmMatch = Map.Make (String)
module CtxMatch = Map.Make (String)

(* Matches a formula according to the generic formula seq_fm *)

let rec match_fm acc seq_fm fm =
  match (seq_fm, fm) with
  | Val a, Val b when a = b -> (false, acc)
  | Atm (nm, []), _ when String.starts_with ~prefix:"?A" nm ->
    (true, FmMatch.add nm [ fm ] acc)
  | Atm (nm_a, args_a), Atm (nm_b, args_b)
    when nm_a = nm_b && List.length args_a = List.length args_b -> (false, acc)
  | Op (op_a, args_a), Op (op_b, args_b)
    when op_a = op_b && List.length args_a = List.length args_b ->
    List.fold_right2
      (fun el_a el_b (mut, acc) ->
        let mut', acc' = match_fm acc el_a el_b in
        (mut || mut', acc'))
      args_a
      args_b
      (false, acc)
  | Qt (qt_a, x, fm_a), Qt (qt_b, y, fm_b) when qt_a = qt_b && x = y ->
    match_fm acc fm_a fm_b
  | _ -> (false, acc)

(* Does a formula substitution according to a given binding *)

let subst_fm mtc cxt fm =
  match fm with
  | Atm (nm, _) -> (
    let subst = FmMatch.find_opt nm mtc in
    match subst with
    | None -> fm :: cxt
    | Some s -> s @ cxt)
  | _ -> fm :: cxt

let subst_seq mtc seq =
  Sequent.map
    (fun fm_li -> List.fold_right (fun fm acc -> subst_fm mtc acc fm) fm_li [])
    seq

(* Applies the given rule to the sequent *)

let apply rule seq =
  (* Matches the formulae of the sequent and binds the variables of the rule
     with them *)
  let mtc, ctc =
    Sequent.fold
      (fun k v (mtc, ctc) ->
        let fm_seq = Sequent.find_opt k seq in
        match fm_seq with
        | None -> (mtc, CtxMatch.add (fmt_cxt k) [] ctc)
        | Some fm_li ->
          (* Gets the new matching + the context that consists in all formula
             that couldn't match the generic formula *)
          List.fold_right
            (fun el (acc, bcc) ->
              let cxt, mtc' =
                List.fold_right
                  (fun fm (cxt, acc) ->
                    let mut, acc' = match_fm acc el fm in
                    (* Printf.printf "%b -- %s\n" mut (show_fm fm); *)
                    if not mut then (fm :: cxt, acc) else (cxt, acc'))
                  fm_li
                  ([], acc)
              in
              (mtc', CtxMatch.add (fmt_cxt k) cxt bcc))
            v
            (mtc, ctc))
      rule.csq
      (FmMatch.empty, CtxMatch.empty)
  in
  (* If the match is empty, then the rule is not suited for the sequent,
     otherwise, it returns the match *)
  if FmMatch.is_empty mtc then None
  else Some (List.map (subst_seq ctc) (List.map (subst_seq mtc) rule.hyp))

(* Automatic search for suited rules *)

let show_auto () = "[\x1B[33mauto\x1B[0m]"

let rec auto log rules seq =
  if is_tautology seq log then (
    Printf.printf "\n %s\tapply axiom." (show_auto ());
    true)
  else
    let rec go = function
      | [] -> false
      | rule :: t -> (
        match apply rule seq with
        | None -> go t
        | Some li ->
          Printf.printf "\n %s\tapply %s." (show_auto ()) rule.name;
          List.for_all (auto log rules) li)
    in
    go rules
