
%{
    open Syntax
    open Formula
%}

%token THEOREM PROOF APPLY QED
%token DOT COMMA COLUMN L_BRACE R_BRACE
%token<string> NM
%token EOF

%start<Syntax.program> lug

%%
app: APPLY ; rule_name = NM ; DOT {rule_name}

formula:
    | atm_name = NM {Atm (atm_name, [])}
    | op_name = NM ; L_BRACE ; fms = separated_list(COMMA, formula) ; R_BRACE {Op (op_name, fms)}
    // | qt_name = NM ; L_BRACE ; var = NM ; R_BRACE ; DOT ; fm = formula {Qt (qt_name, var, fm)}
thm:
    THEOREM ; thm_name = NM ; COLUMN ; fm = formula ; DOT ; 
    PROOF ; DOT ; rules = list(app) ; QED ; DOT   { { name=thm_name ; stmt = fm ; proof = rules } }

lug:
    thms=list(thm) EOF { {thms} }
%%