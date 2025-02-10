(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

   val token : Lexing.lexbuf -> Parser.token
exception SyntaxError of string