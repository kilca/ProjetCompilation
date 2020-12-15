type opComp =
  Eq | Neq | Lt | Le | Gt | Ge


type const=
  Int of int
| String of string


type expType =
  Id of string
| ClassID of string
| Cste of const
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| And of expType*expType
| UMinus of expType
| UPlus of expType
| Comp of opComp*expType*expType
| Cast of string*expType
| Selec of expType*string (*recuperation d'attribut*)
| Call of expType*string*expType list
| Inst of string*expType list
| None (**)

type decl = {
    lhs: string;
    typ: string; (*string ou defType ?*)
    isVar: bool;
    rhs: expType option;(*attention !!!! optionnel ou potentiellement Null*)
  }
  


type paramDecl = decl list


type blocType = decl list*instr list
and
instr =
 Expr of expType
| Bloc of blocType
| Return of expType
| Ite of expType*instr*instr
| Assign of expType*expType


type superO ={
  ex : string;
  para : expType list;
}

type consDecl={
  nom : string;
  para: paramDecl;
  superrr : superO option;
  bloc : blocType;
}

type funDecl={
  nom : string;
  para: paramDecl;
  typ : string option; (*type de retour*) (*attention !!!! optionnel*)
  (*argType : expType list option; liste d'argument du constructeur parent*)
  over : bool;
  corp : instr;
}

type confun =
 Fun of funDecl
| Con of consDecl


type classBloc ={
  dec : decl list;
  fon : confun list;
}


type classDecl = {
  nom : string;
  para : paramDecl;
  ext : string option;(*attention !!! optionnel*)
  cbl : classBloc;
  }

type objetDecl = {
  nom : string;
  dec : decl list;
  fon : funDecl list;
}

(* type block = instr list; *)

type classObjDecl =
  Class of classDecl
  |Objet of objetDecl

type progType = classObjDecl list*blocType
