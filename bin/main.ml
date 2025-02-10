(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

open Lug.Ker

let inputs = ref []
let spec = []
let handler fname = inputs := fname :: !inputs
let usage = "lug <file>"

let () = 
  Arg.parse spec handler usage;
  List.iter (fun fname -> exec fname) !inputs