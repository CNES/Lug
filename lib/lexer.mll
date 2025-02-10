{
    open Parser
    exception SyntaxError of string
}

let white = ' ' | '\t' | '\r' | '\n' | "\r\n"
let nm = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
    | white+        { token lexbuf }
    | "Theorem"     { THEOREM }
    | "Proof"       { PROOF }
    | "apply"       { APPLY }
    | "Qed"         { QED }
    | '.'           { DOT }
    | ','           { COMMA }
    | ':'           { COLUMN }
    | '('           { L_BRACE }
    | ')'           { R_BRACE }
    | nm            { NM(Lexing.lexeme lexbuf) }
    | eof           { EOF }
    | _             { raise (SyntaxError ("Unknown syntax : " ^ Lexing.lexeme lexbuf))}