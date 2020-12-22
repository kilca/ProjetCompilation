(* tpParse.mly *)
%{
open Ast
%}
%token <string> ID
%token <string> CLASSID
%token <Ast.const> CSTE

%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token CONCAT
%token LPAREN RPAREN SEMICOLON
%token ASSIGN
%token OBJECT
%token DEF

%token IF THEN ELSE

%token CLASS EXTENDS IS
%token COMMA LACCO RACCO
%token VAR
%token DOT
%token COLON
%token NEW
%token RETURN
%token OVERRIDE
%token AS
/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS
%token UPLUS /*je trouve pas ca utile mais bon */

%token EOF

%nonassoc RELOP
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */ 
%left UMINUS UPLUS           /* highest precedence */
%left CONCAT            /* peu importe la precedence car n'est pas compatible avec les op de type int */
%left DOT
(* %right COLON *)

/* il ya peut etre d'autre types type precisable mais pas besoin */
%type <classObjDecl> classeobj 
%type <expType> expr opt_expr
%type <decl> declaration
%type <classDecl> class_declaration
%type <objetDecl> objet_declaration
%type <funDecl> fun_declaration
%type <consDecl> con_declaration
%type <superO> superr
%type <blocType> bloc

%start<Ast.progType> prog
%%

(* entierete du prog avec main *)
prog:  lc=list(classeobj) b=bloc EOF {lc,b}

(*soit code classe, soit code objet *)
classeobj :
  x = class_declaration {Class(x)}
  | x = objet_declaration {Objet(x)}

(*initialisation de variable (le new est dans exp)*)
declaration_init :
ASSIGN e = expr {e}


(* bloc d'instruction *)
bloc :
 LACCO l = list(instruction) RACCO {[], l}
 | LACCO ld =nonempty_list(variable) IS l = nonempty_list(instruction) RACCO {ld, l}


declaration_instr :
d=declaration SEMICOLON {d}

(* variable defini dans bloc *)
variable :
  x = ID COLON ty=CLASSID e = option(declaration_init) SEMICOLON
  { { lhs = x;typ=ty;isVar=false; rhs = e; } }

(*variable defini dans parametre *)
declaration : 
  b=boption(VAR) x = ID COLON ty=CLASSID e = option(declaration_init)
  { { lhs = x;typ=ty;isVar=b; rhs = e; } }

(*les expressions du langage*)
expr:
    x = ID                        { Id x }
  | v = CSTE                      { Cste v }
  (* OPERATEURS : *)
  | g = expr PLUS d = expr        { Plus (g, d) }
  | g = expr MINUS d = expr       { Minus (g, d) }
  | g = expr TIMES d = expr       { Times (g, d) }
  | g = expr DIV d = expr         { Div (g, d) }
  | g = expr CONCAT d = expr         { Concat (g, d) }
  | PLUS e = expr %prec UPLUS       { UPlus e }
  | MINUS e = expr %prec UMINUS   { UMinus e }
  | g = expr op = RELOP d = expr  { Comp(op, g, d) }
  (* PARENTHESES ET AUTRES : *)
  | LPAREN AS x=CLASSID COLON e=expr RPAREN { Cast (x, e) }
  (*| s=selexpr {s}*)
  | e= expr DOT i=ID {Selec(e,i)}
  | e = delimited (LPAREN, expr, RPAREN) { e }
  | x=expr DOT i=ID param=delimited(LPAREN,separated_list(COMMA,expr),RPAREN) { Call (x,i,param) }
  | x=CLASSID DOT i=ID param=delimited(LPAREN,separated_list(COMMA,expr),RPAREN)   { Call (Cste(String(x)),i,param) } (* ou plutot Cste->ID ? *)
  | NEW i=CLASSID param=delimited(LPAREN,separated_list(COMMA,expr),RPAREN) {Inst(i,param)}

(*le extends de classe *)
opt_ext: EXTENDS e=CLASSID{e}

(*declaration globale de classe *)
class_declaration :
    CLASS n=CLASSID p = delimited (LPAREN,separated_list(COMMA,declaration),RPAREN) ex=option(opt_ext) c=class_bloc 
    {{nom=n;para=p;ext=ex;cbl=c}}

(*soit constructeur soit fonction *)
confun:
 x =con_declaration {Con(x)}
| x = fun_declaration {Fun (x)}
| x = declaration_instr {Att (x)}

(*declaration du corp/bloc de la classe *)
class_bloc: (*bloc de la classe *)
  IS LACCO decs=list(confun) RACCO 
  {{dec=decs}}

(*declaration globale d'un objet *)
objet_declaration:
    OBJECT n=CLASSID IS LACCO ld =list(declaration_instr) func=list(fun_declaration) RACCO 
    {{nom=n;dec =ld;fon=func}}

(*definission du type de retour de fonction (optionnel)*)
opt_type : COLON ty=CLASSID {ty}

(*declaration de fonction au sein d'une classe/objet *)
fun_declaration :
  DEF ov=boption(OVERRIDE) n = ID p = delimited(LPAREN,separated_list(COMMA,declaration),RPAREN) o=option(opt_type) IS blo=bloc
  {{nom= n;para=p;typ=o;over=ov;corp= Bloc (blo)}}
  | DEF ov=boption(OVERRIDE) n = ID p = delimited(LPAREN,separated_list(COMMA,declaration),RPAREN) o=option(opt_type) ASSIGN i=expr
  {{nom= n;para=p;typ=o;over=ov;corp= Bloc([],[Return (Some i)] ) }}

(*declaration de constructeur*)
con_declaration :
DEF x = CLASSID a = delimited(LPAREN,separated_list(COMMA,declaration),RPAREN) b = option(superr) IS blo= bloc
{{nom = x;para = a; superrr = b; bloc = blo}}

(*super du constructeur*)
superr :
COLON y = CLASSID p = delimited(LPAREN,separated_list(COMMA,expr),RPAREN) {{ex = y ; para = p}}
 
opt_expr:
x = expr{x}
(*instructions du langage (possible de preciser + cas mais possible VC)*)
instruction :
  x = expr SEMICOLON { Expr(x) }
  | x = bloc {Bloc (x)}
  |RETURN x=option(opt_expr) SEMICOLON {Return(x)}
  |g = expr ASSIGN d = expr SEMICOLON  {Assign(g,d)}
  |IF a = expr THEN b = instruction ELSE c = instruction
   {Ite(a,b,c)}
