(*tpParse.mly*)
%{
open Ast
%}
%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMICOLON
%token ASSIGN
%token OBJECT
%token DEF SUPERCLASS



%token IF THEN ELSE BEGIN END

%token CLASS SUPER EXTENDS IS
%token COMMA LACCO RACCO
%token VAR
%token DOT

/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS

%token EOF

%right ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */

%type <expType> expr bexpr
%type <decl> declaration

%type <expType list> exprList

%start<Ast.progType> prog
%%

(*entierete du prog avec main *)
prog:  lc=list(classe) LACCO ld =list(declaration) IS i = expr RACCO EOF {lc,ld,i}

declaration : 
  x = ID ASSIGN e = expr SEMICOLON
  { { lhs = x; rhs = e; } }
  | x = ID ASSIGN NEW 

exprList:
  e= expr {e}
  | expr COMMA exprList {...}

expr:
    x = ID                        { Id x }
  | v = CSTE                      { Cste v }
  | g = expr PLUS d = expr        { Plus (g, d) }
  | g = expr MINUS d = expr       { Minus(g, d) }
  | g = expr TIMES d = expr       { Times(g, d) }
  | g = expr DIV d = expr         { Div(g, d) }
  | PLUS e = expr                 { e }
  | MINUS e = expr %prec UMINUS   { UMinus e }
  | e = delimited (LPAREN, expr, RPAREN) { e }
  | IF si=bexpr THEN alors=expr ELSE sinon = expr
    { Ite(si, alors, sinon) }
  | x=ID e=delimited (LPAREN, exprList, RPAREN) {x,e}
  (*| ID DOT x=ID {Id x}*)

bexpr : (*bool expr du if *)
    g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | e = delimited (LPAREN, bexpr, RPAREN) { e }

bloc:(*bloc de fonction*)
    LACCO ld =list(declaration) IS i = expr RACCO EOF 
    {ld, i }
  | LACCO i = expr RACCO EOF {i}(*probleme ici null?*)

classeDecl
    CLASS n=ID param EXTENDS c=ID IS classeBloc 
    {{}}
  | CLASS n=ID param EXTENDS IS classeBloc {}


classeBloc: (* classe *)
  LACCO ld =list(declaration) IS i = expr RACCO EOF {lc, ld, i }

def : (* definition de fonction *)
  DEF n = ID p = delimited (LPAREN,param,RPAREN) IS { instr }
  | DEF n = ID LPAREN,param:SUPERCLASSE larg IS { instr }

params: (*definition de tous les parametres *)
   e = delimited (LPAREN, lparam, RPAREN) { e }

lparam: (*definition d'un parametre *)
  VAR v=ID{...}
  | VAR v=ID COMMA l=lparam
  | v=ID {}
  | v=ID COMMA l=lparam
