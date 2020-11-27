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
%token DEF



%token IF THEN ELSE

%token CLASS EXTENDS IS AS
%token COMMA LACCO RACCO
%token VAR
%token DOT
%token DEFTYPE
%token DOUBLEPOINT
%token NEW
%token RETURN (*A VOIR SI Existe*)

/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS

%token EOF

%right ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */

%type <classObjDecl> classeobj 
%type <expType> expr bexpr declaration_init
%type <decl> declaration
%type <decl list> params
%type <expType list> exprList
%type <classDecl> class_declaration
%type <objetDecl> objet_declaration

%start<Ast.progType> prog
%%

(*entierete du prog avec main *)
prog:  lc=list(classeobj) LACCO ld =list(declaration) IS i = expr RACCO EOF {lc,ld,i}

classeobj :
  x = class_declaration {Class(x)}
  | x = objet_declaration {Objet(x)}

declaration_init :
ASSIGN e = expr {e}
| ASSIGN NEW e = expr {e}
| {None}

declaration : 
  x = ID DOUBLEPOINT ty=DEFTYPE e = declaration_init SEMICOLON
  { { lhs = x;typ=ty;isVar=false; rhs = e; } }
  | VAR x = ID DOUBLEPOINT ty=DEFTYPE e = declaration_init SEMICOLON
  { { lhs = x;typ=ty;isVar=true; rhs = e; } }

exprList:
  e= expr {[e]}
  | e=expr COMMA s=exprList {s::e}
  |{[]}

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
  | x=ID e=delimited (LPAREN, exprList, RPAREN) {x,e} (* appel fonction *)
  | x=ID DOT e=expr {Call(x,e)} (*select *)
  | AS x=ID DOUBLEPOINT e=expr {Cast(x,e)} (*cast *)
  | RETURN e = expr {Return(e)} (*A VOIR SI EXISTE*)

bexpr : (*bool expr du if *)
    g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | e = delimited (LPAREN, bexpr, RPAREN) { e }

fun_bloc:(*bloc de fonction*)
    IS LACCO ld =list(declaration) IS e = expr RACCO EOF 
    {ld, e }
  | IS LACCO e = expr RACCO EOF {[],e}
  | ASSIGN e=expr {[],e}

class_declaration :
    CLASS n=ID p = delimited (LPAREN,params,RPAREN) c=class_bloc 
    {{nom=n;para=p;ext=None;sup=None;cbl=c}}
    | CLASS n=ID p = delimited (LPAREN,params,RPAREN) EXTENDS ex=ID class_bloc 
    {{nom=n;para=p;ext=ex;cbl=c}}


class_bloc: (*bloc de la classe *)
  IS LACCO ld =list(declaration) con=fun_declaration func=list(fun_declaration) RACCO 
  {dec=ld;cons=con;fon=func;}

objet_declaration:
    OBJECT n=ID IS LACCO ld =list(declaration) func=list(fun_declaration) RACCO 
    {{nom=n;dec =ld;fon=func}}


fun_declaration :
  DEF n = ID p = delimited (LPAREN,params,RPAREN) blo=fun_bloc
  {{nom= n;para=p;typ=None;bloc= blo;}}
  | DEF n = ID p = delimited (LPAREN,params,RPAREN) DOUBLEPOINT ty=DEFTYPE blo=fun_bloc
  {{nom= n;para=p;typ=ty;bloc= blo;}}
  (*cas constructeur *)
  | DEF n = ID p = delimited (LPAREN,params,RPAREN) DOUBLEPOINT i=ID p2=delimited(LPAREN,params,RPAREN) blo=fun_bloc
  {{nom= n;para=p;typ=None;bloc= Call(i,Fun(i,p2)) :: blo}}
  | DEF n = ID p = delimited (LPAREN,params,RPAREN) DOUBLEPOINT i=ID p2=delimited(LPAREN,params,RPAREN) blo=fun_bloc
  {{nom= n;para=p;typ=None;bloc= Call(i,Fun(i,p2)) :: blo}}

params: (*definition des parametres *)
  d=declaration {d}
  |{[]}
  | d=declaration COMMA p=params {p::d}
