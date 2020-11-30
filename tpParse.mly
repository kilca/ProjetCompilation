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

%token CLASS EXTENDS IS
%token COMMA LACCO RACCO
%token VAR
%token DOT
%token DEFTYPE
%token COLON
%token NEW
%token RETURN (*A VOIR SI Existe*)
%token OVERRIDE
%token AS
/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS

%token EOF


%right ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */
%left DOT 				/*je trouve la regle mal ecrite*/
%right COLON
%left RETURN
						/*reste un conflit mais je trouve pas comment le resoudre*/


%type <classObjDecl> classeobj 
%type <expType> expr bexpr declaration_init
%type <decl> declaration
%type <decl list> params
%type <expType list> exprList
%type <classDecl> class_declaration
%type <objetDecl> objet_declaration
%type <funDecl> fun_declaration fun_declaration_over

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
  x = ID COLON ty=DEFTYPE e = declaration_init SEMICOLON
  { { lhs = x;typ=ty;isVar=false; rhs = e; } }
  | VAR x = ID COLON ty=DEFTYPE e = declaration_init SEMICOLON
  { { lhs = x;typ=ty;isVar=true; rhs = e; } }

exprList:
  e= expr {[e]}
  | e=expr COMMA s=exprList {s::e}
  |{[]}

expr:
    x = ID                        { Id x }
  | v = CSTE                      { Cste v }
  | g = expr PLUS d = expr        { Plus (g, d) }
  | g = expr MINUS d = expr       { Minus (g, d) }
  | g = expr TIMES d = expr       { Times (g, d) }
  | g = expr DIV d = expr         { Div (g, d) }
  | PLUS e = expr                 { e }
  | MINUS e = expr %prec UMINUS   { UMinus e }
  | RETURN e = expr {Return(e)} (*A VOIR SI EXISTE*)
  | e = delimited (LPAREN, expr, RPAREN)            { e }
  | LPAREN AS x=ID COLON e=expr RPAREN { Cast (x, e) }
  | IF si=bexpr THEN alors=expr ELSE sinon = expr   { Ite (si, alors, sinon) }
  | x=ID e=delimited (LPAREN, exprList, RPAREN)     { x, e } (* appel fonction *)
  | x=ID DOT e=expr                                 { Call (x,e) }

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
  | DEF n = ID p = delimited (LPAREN,params,RPAREN) COLON ty=DEFTYPE blo=fun_bloc
  {{nom= n;para=p;typ=ty;bloc= blo;}}
  | f=con_declaration {f}
  | f=fun_declaration_over {f}

con_declaration :
   DEF n = ID p = delimited (LPAREN,params,RPAREN) COLON i=ID p2=delimited(LPAREN,params,RPAREN) blo=fun_bloc
  {{nom= n;para=p;typ=None;bloc= Call(i,Fun(i,p2)) :: blo}}
  | DEF n = ID p = delimited (LPAREN,params,RPAREN) COLON i=ID p2=delimited(LPAREN,params,RPAREN) blo=fun_bloc
  {{nom= n;para=p;typ=None;bloc= Call(i,Fun(i,p2)) :: blo}}


  instruction :
x = expr SEMICOLON { ex(x} }
| x = fun_bloc {Bloc(x)}
|RETURN x = expr SIMICOLON {R(x)}
|g = ID ASSIGN d = expr SEMICOLON  {X(g,d)}
|IF a = expr THEN b = instruction ELSE c = instruction SEMICOLON
 {Ite(a,b,c)}
| e = delimited(LPAREN, expr, RPAREN) {e}

fun_declaration_over : 
  DEF OVERRIDE n = ID p = delimited (LPAREN,params,RPAREN) blo=fun_bloc
  {{nom= n;para=p;typ=None;bloc= blo}}
  | DEF OVERRIDE n = ID p = delimited (LPAREN,params,RPAREN) COLON ty=DEFTYPE blo=fun_bloc
  {{nom= n;para=p;typ=ty;bloc= blo}}


params: (*definition des parametres *)
  d=declaration {d}
  |{[]}
  | d=declaration COMMA p=params {p::d}
