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
%nonassoc RELOP
/*%left DOT Etrange pas besoin de preciser */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */
(* %right COLON *)
						/*reste un conflit mais je trouve pas comment le resoudre*/


%type <classObjDecl> classeobj 
%type <expType> expr expr2
%type <declInit> declaration_init
%type <decl> declaration
%type <decl list> params
%type <expType list> exprList
%type <classDecl> class_declaration
%type <objetDecl> objet_declaration
%type <funDecl> fun_declaration
%type <consDecl> con_declaration
(*%type <superO> superr*)
%type <blocType> bloc

%start<Ast.progType> prog
%%

(*entierete du prog avec main *)
prog:  lc=list(classeobj) b=bloc EOF {lc,b}

classeobj :
  x = class_declaration {Class(x)}
  | x = objet_declaration {Objet(x)}

declaration_init :
ASSIGN e = expr {VarInit(e)}
| ASSIGN NEW i=ID param=delimited(LPAREN,exprList,RPAREN) {ClassInit(i,param)}

(*ne pas def %type ici *)
bloc : LACCO ld =list(declaration) IS l = list(instruction) RACCO {ld, l}

declaration : 
  b=boption(VAR) x = ID COLON ty=DEFTYPE e = option(declaration_init) SEMICOLON
  { { lhs = x;typ=ty;isVar=b; rhs = e; } }


exprList:
  e= expr {[e]}
  | e=expr COMMA s=exprList {e::s}
  |{[]}


expr2 :
x = ID {Id x}

expr:
    x = ID                        { Id x }
  | v = CSTE                      { Cste v }
  | g = expr PLUS d = expr        { Plus (g, d) }
  | g = expr MINUS d = expr       { Minus (g, d) }
  | g = expr TIMES d = expr       { Times (g, d) }
  | g = expr DIV d = expr         { Div (g, d) }
  | PLUS e = expr                 { e }
  | MINUS e = expr %prec UMINUS   { UMinus e }
  | g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | e = delimited (LPAREN, expr, RPAREN)            { e }
  | LPAREN AS x=ID COLON e=expr RPAREN { Cast (x, e) }
  | s=selexpr {s}
  (*| e= expr DOT i=ID {Selec(e,i)}*)
  | x=ID DOT i=ID param=delimited(LPAREN,exprList,RPAREN) { Call (x,i,param) }
  | x=CLASSID DOT i=ID param=delimited(LPAREN,exprList,RPAREN)   { Call (x,i,param) } 
(*probleme avec cas call cast entre parenthese*)

selexpr:
e= ID DOT i=ID {Selec(Id(e),i)}
|e=selexpr DOT i=ID {Selec(e,i)}




(* plus besoin (cf sujet)
bexpr : 
    g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | e = delimited (LPAREN, bexpr, RPAREN) { e }
*)

opt_ext: EXTENDS e=CLASSID{e}

class_declaration :
    CLASS n=CLASSID p = delimited (LPAREN,params,RPAREN) ex=opt_ext c=class_bloc 
    {{nom=n;para=p;ext=ex;cbl=c}}

class_bloc: (*bloc de la classe *)
  IS LACCO ld =list(declaration) con=con_declaration func=list(fun_declaration) RACCO 
  {{dec=ld;cons=con;fon=func;}}

objet_declaration:
    OBJECT n=ID IS LACCO ld =list(declaration) func=list(fun_declaration) RACCO 
    {{nom=n;dec =ld;fon=func}}

opt_type : COLON ty=DEFTYPE {ty}

fun_declaration :
  DEF ov=boption(OVERRIDE) n = ID p = delimited(LPAREN,params,RPAREN) o=option(opt_type) IS blo=bloc
  {{nom= n;para=p;typ=o;over=ov;corp= Bloc (blo);}}
  | DEF ov=boption(OVERRIDE) n = ID p = delimited(LPAREN,params,RPAREN) o=option(opt_type) ASSIGN i=instruction
  {{nom= n;para=p;typ=o;over=ov;corp= i;}}

(*a modifier (ai juste enleve les erreurs)*)
con_declaration :
DEF x = CLASSID a = params b = boption(superr) blo= bloc
{{nom = x;para = a; superrr = b; bloc = blo}}

superr :
COLON y = expr2 p = params {{ex = y ; para = p}}
 

instruction :
  x = expr SEMICOLON { Expr(x) }
  | x = bloc {Bloc (x)}
  |RETURN x = expr SEMICOLON {Return(x)}
  |g = ID ASSIGN d = expr SEMICOLON  {Assign(g,d)}
  |IF a = expr THEN b = instruction ELSE c = instruction
   {Ite(a,b,c)}

params: (*definition des parametres *)
  d=declaration {[d]}
  |{[]}
  | d=declaration COMMA p=params {d::p}
