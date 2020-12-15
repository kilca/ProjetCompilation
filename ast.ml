(*comparateur*)
type opComp =
  Eq | Neq | Lt | Le | Gt | Ge

(*constante (type defini dans lex)*)
type const=
  Int of int
| String of string

(*expression (2+2) *)
type expType =
  Id of string (*commence forcemment par minuscule *)
| ClassID of string (*et objet, commence forcemment par majuscule *)
| Cste of const
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| And of expType*expType(*concat string*)
| UMinus of expType
| Comp of opComp*expType*expType
| Cast of string*expType
| Selec of expType*string (*recuperation d'attribut*)
| Call of expType*string*expType list
| Inst of string*expType list

(*declaration de variable/attribut *)
type decl = {
    lhs: string;
    typ: string; 
    isVar: bool;
    rhs: expType option;(*attention !!!! optionnel *)
  }
  
(*liste de declaration (parametres) *)
type paramDecl = decl list

(*bloc d'instruction et instructions *)
type blocType = decl list*instr list
and
instr =
 Expr of expType
| Bloc of blocType
| Return of expType option
| Ite of expType*instr*instr
| Assign of expType*expType

(*appel du constructeur super *)
type superO ={
  ex : string;
  para : expType list;
}

(*declaration d'un constructeur*)
type consDecl={
  nom : string;
  para: paramDecl;
  superrr : superO option;
  bloc : blocType;
}

(*declaration d'une fonction *)
type funDecl={
  nom : string;
  para: paramDecl;
  typ : string option; (*type de retour*) (*attention !!!! optionnel*)
  over : bool;
  corp : instr;
}

(*PENSER A Verif en VC*)
(*differents membres de classe (Fonction, Constructeur, Attribut)*)
type membreClasse =
 Fun of funDecl
| Con of consDecl
| Att of decl

(*bloc d'une classe class{...} *)
type classBloc ={
  dec : membreClasse list;
}

(*declaration d'en tete d'une classe *)
type classDecl = {
  nom : string;
  para : paramDecl;
  ext : string option;(*attention !!! optionnel*)
  cbl : classBloc;
  }

(*declaration d'en tete d'un objet *)
type objetDecl = {
  nom : string;
  dec : decl list;
  fon : funDecl list;
}

(*classe ou objet (pour main) *)
type classObjDecl =
  Class of classDecl
  |Objet of objetDecl

(*programme principal *)
type progType = classObjDecl list*blocType
