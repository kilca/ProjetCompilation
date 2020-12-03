(*tpParse.mly*)
%{
open Ast
%}
%token <string> ID
%token <string> CLASSID
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
%token <Ast.defType>DEFTYPE
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

//%left RETURN
//%right ELSE
//%left DOT 				/*je trouve la regle mal ecrite*/
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */
//%right COLON
						/*reste un conflit mais je trouve pas comment le resoudre*/


%type <classObjDecl> classeobj 
%type <expType> expr bexpr declaration_init
%type <decl> declaration
%type <decl list> params
%type <expType list> exprList
%type <classDecl> class_declaration
%type <objetDecl> objet_declaration
%type <funDecl> fun_declaration

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

(*ne pas def %type ici *)
bloc : LACCO ld =list(declaration) IS l = list(instruction) RACCO EOF {ld, l}

declaration : 
  x = ID COLON ty=DEFTYPE e = option(declaration_init) SEMICOLON
  { { lhs = x;typ=ty;isVar=false; rhs = e; } }
  | VAR x = ID COLON ty=DEFTYPE e = option(declaration_init) SEMICOLON
  { { lhs = x;typ=ty;isVar=true; rhs = e; } }


exprList:
  e= expr {[e]}
  | e=expr COMMA s=exprList {e::s}
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
  | e = delimited (LPAREN, expr, RPAREN)            { e }
  | LPAREN AS x=ID COLON e=expr RPAREN { Cast (x, e) }
  | x=ID DOT i=ID param=delimited(LPAREN,exprList,RPAREN) { Call (x,i,param) }
  | x=CLASSID DOT i=ID param=delimited(LPAREN,exprList,RPAREN)   { Call (x,i,param) } 
  (* Objet.func() attention peut amener erreur*)
(*probleme avec cas call cast entre parenthese*)

bexpr : (*bool expr du if *)
    g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | e = delimited (LPAREN, bexpr, RPAREN) { e }

opt_ext: EXTENDS e=CLASSID{e}

class_declaration :
    CLASS n=CLASSID p = delimited (LPAREN,params,RPAREN) ex=option(opt_ext) c=class_bloc 
    {{nom=n;para=p;ext=ex;cbl=c}}

class_bloc: (*bloc de la classe *)
  IS LACCO ld =list(declaration) con=con_declaration func=list(fun_declaration) RACCO 
  {{dec=ld;cons=con;fon=func;}}

objet_declaration:
    OBJECT n=ID IS LACCO ld =list(declaration) func=list(fun_declaration) RACCO 
    {{nom=n;dec =ld;fon=func}}

opt_overr : OVERRIDE n=ID {n}
opt_type : COLON ty=DEFTYPE {ty}

fun_declaration :
  DEF ov=boption(opt_overr) n = ID p = delimited(LPAREN,params,RPAREN) o=option(opt_type) IS blo=bloc
  {{nom= n;para=p;typ=o;over=ov;blocf= blo;}}

(*a modifier (ai juste enleve les erreurs)*)
con_declaration : 
   DEF n = CLASSID p = delimited (LPAREN,params,RPAREN) COLON i=ID p2=delimited(LPAREN,params,RPAREN) IS blo=bloc
  (*
  {{nom= n;para=p;typ=None;blocf= Call(i,Fun(i,p2)) :: blo}}
  *)
    {{nom= n;para=p;over=false;typ=None;blocf= blo}}
  | DEF n = CLASSID p = delimited (LPAREN,params,RPAREN)  blo=bloc
  {{nom= n;para=p;over=false;typ=None;blocf= blo}}
  

instruction :
  x = expr SEMICOLON { Expr(x) }
  | x = bloc {Bloc (x)}
  |RETURN x = expr SEMICOLON {Return(x)}
  |g = ID ASSIGN d = expr SEMICOLON  {Assign(g,d)}
  |IF a = bexpr THEN b = instruction ELSE c = instruction
   {Ite(a,b,c)}

params: (*definition des parametres *)
  d=declaration {[d]}
  |{[]}
  | d=declaration COMMA p=params {d::p}
