(* Copyright

   Vladislas de Haldat, Space Transportation Directorate, CNES

   August 2024

   Released under the terms of CeCILL license (see file LICENSE) *)

let parse fname =
  let input = open_in fname in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
  let prog =
    try Parser.lug Lexer.token lexbuf with
    | Lexer.SyntaxError e ->
      Printf.printf "%s\n" e;
      {thms = []}
    | Parser.Error ->
      Printf.printf "Parser error %d\n" (lexbuf.lex_curr_p.pos_lnum);
      {thms = []}
    in
  close_in input;
  prog