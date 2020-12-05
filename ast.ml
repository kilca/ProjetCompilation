type opComp =
  Eq | Neq | Lt | Le | Gt | Ge


(*A supprimer ?*)

type defType=string

(*
type defType=
  Integer
  |String
  |Function of string
  *) (*Id de fonction ou classe*)


type expType =
  Id of string
| ClassID of string
| Cste of int
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| UMinus of expType
| Comp of opComp*expType*expType
(*| Ite of expType*expType*expType*)
| Cast of string*expType
| Selec of expType*string (*recuperation d'attribut*)
| Call of string*string*expType list
| None (*A voir si on garde ou pas, chances que non*)

(* Modifications *)

type decl = {
    lhs: string;
    typ: defType; (*string ou defType ?*)
    isVar: bool;
    rhs: expType option;(*attention !!!! optionnel ou potentiellement Null*)
  }

type paramDecl = decl list


(*Probleme ici, ne marche pas*)
type blocType = decl list*instr list
and
instr =
 Expr of expType
| Bloc of blocType
| Return of expType
| Ite of expType*instr*instr
| Assign of string*expType


type consDecl={
  nom : string;
  para: paramDecl;
  bloc : blocType;
}

type funDecl={
  nom : string;
  para: paramDecl;
  typ : defType option; (*type de retour*) (*attention !!!! optionnel*)
  (*argType : expType list option; liste d'argument du constructeur parent*)
  over : bool;
  blocf : blocType;
}

type classBloc ={
  dec : decl list;
  cons : consDecl;
  fon : funDecl list;

}


type classDecl = {
  nom : string;
  para : paramDecl;
  ext : string;(*attention !!! optionnel*)
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
