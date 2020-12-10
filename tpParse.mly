(* tpParse.mly *)
%{
open Ast
%}
%token <string> ID
%token <string> CLASSID
%token <Ast.const> CSTE

%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
(*%token AND*)
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
%token RETURN (* A VOIR SI Existe *)
%token OVERRIDE
%token AS
/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS

%token EOF

/*
%left RETURN
%right ELSE
*/
%nonassoc RELOP
%left DOT
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */ 
%left UMINUS            /* highest precedence */
(* %right COLON *)
						/*reste un conflit mais je trouve pas comment le resoudre*/


%type <classObjDecl> classeobj 
%type <expType> expr
(*
%type <declInit> declaration_init
*)
%type <decl> declaration
(*
%type <decl list> params
%type <expType list> exprList
*)
%type <classDecl> class_declaration
%type <objetDecl> objet_declaration
%type <funDecl> fun_declaration
%type <consDecl> con_declaration
(*%type <superO> superr*)
%type <blocType> bloc

%start<Ast.progType> prog
%%

(* entierete du prog avec main *)
prog:  lc=list(classeobj) b=bloc EOF {lc,b}

classeobj :
  x = class_declaration {Class(x)}
  | x = objet_declaration {Objet(x)}


declaration_init :
ASSIGN e = expr {e}


(* ne sais pas entre list instr et list decl lorsque ID *)
bloc :
 LACCO l = list(instruction) RACCO {[], l}
 | LACCO ld =nonempty_list(variable) IS l = nonempty_list(instruction) RACCO {ld, l}

declaration_instr :
d=declaration SEMICOLON {d}


variable :
  x = ID COLON ty=CLASSID e = option(declaration_init) SEMICOLON
  { { lhs = x;typ=ty;isVar=false; rhs = e; } }

declaration : 
  b=boption(VAR) x = ID COLON ty=CLASSID e = option(declaration_init)
  { { lhs = x;typ=ty;isVar=b; rhs = e; } }

expr:
    x = ID                        { Id x }
  | v = CSTE                      { Cste v }
  (* OPERATEURS : *)
  | g = expr PLUS d = expr        { Plus (g, d) }
  | g = expr MINUS d = expr       { Minus (g, d) }
  | g = expr TIMES d = expr       { Times (g, d) }
  | g = expr DIV d = expr         { Div (g, d) }
  | PLUS e = expr                 { e }
  | MINUS e = expr %prec UMINUS   { UMinus e }
  (*| g = expr AND d = expr         { And (g, d) }*)
  | g = expr op = RELOP d = expr  { Comp(op, g, d) }
  (* PARENTHESES ET AUTRES : *)
  | LPAREN AS x=CLASSID COLON e=expr RPAREN { Cast (x, e) }
  (*| s=selexpr {s}*)
  | e= expr DOT i=ID {Selec(e,i)}
  | e = delimited (LPAREN, expr, RPAREN) { e }
  | x=expr DOT i=ID param=delimited(LPAREN,separated_list(COMMA,expr),RPAREN) { Call (x,i,param) }
  | x=CLASSID DOT i=ID param=delimited(LPAREN,separated_list(COMMA,expr),RPAREN)   { Call (Cste(String(x)),i,param) } (* ou plutot Cste->ID ? *)
  | NEW i=CLASSID param=delimited(LPAREN,separated_list(COMMA,expr),RPAREN) {Inst(i,param)}

opt_ext: EXTENDS e=CLASSID{e}

class_declaration :
    CLASS n=CLASSID p = delimited (LPAREN,separated_list(COMMA,declaration),RPAREN) ex=option(opt_ext) c=class_bloc 
    {{nom=n;para=p;ext=ex;cbl=c}}

confun:
 x =con_declaration {Con(x)}
| x = fun_declaration {Fun (x)}

class_bloc: (*bloc de la classe *)
  IS LACCO ld =list(declaration_instr) func=list(confun) RACCO 
  {{dec=ld;fon=func}}

objet_declaration:
    OBJECT n=CLASSID IS LACCO ld =list(declaration_instr) func=list(fun_declaration) RACCO 
    {{nom=n;dec =ld;fon=func}}

opt_type : COLON ty=CLASSID {ty}

fun_declaration :
  DEF ov=boption(OVERRIDE) n = ID p = delimited(LPAREN,separated_list(COMMA,declaration),RPAREN) o=option(opt_type) IS blo=bloc
  {{nom= n;para=p;typ=o;over=ov;corp= Bloc (blo);}}
  | DEF ov=boption(OVERRIDE) n = ID p = delimited(LPAREN,separated_list(COMMA,declaration),RPAREN) o=option(opt_type) ASSIGN i=expr
  {{nom= n;para=p;typ=o;over=ov;corp= Expr(i);}}

(*a modifier (ai juste enleve les erreurs)*)
con_declaration :
DEF x = CLASSID a = delimited(LPAREN,separated_list(COMMA,declaration),RPAREN) b = option(superr) IS blo= bloc
{{nom = x;para = a; superrr = b; bloc = blo}}

superr :
COLON y = CLASSID p = delimited(LPAREN,separated_list(COMMA,expr),RPAREN) {{ex = y ; para = p}}
 

(*pour assign peut etre mettre les expression toleres (genre 2:=4;)*)
instruction :
  x = expr SEMICOLON { Expr(x) }
  | x = bloc {Bloc (x)}
  |RETURN x = expr SEMICOLON {Return(x)}
  |g = expr ASSIGN d = expr SEMICOLON  {Assign(g,d)}
  |IF a = expr THEN b = instruction ELSE c = instruction
   {Ite(a,b,c)}

(*peut etre remplace par separated list *)
(*
params:
  d=declaration {[d]}
  |{[]}
  | d=declaration COMMA p=params {d::p}
*)
