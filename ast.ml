type opComp =
  Eq | Neq | Lt | Le | Gt | Ge

type defType=
  Integer
  |String
  |Function of string (*Id de fonction ou classe*)


type expType =
  Id of string
| Cste of int
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| UMinus of expType
| Comp of opComp*expType*expType
| Ite of expType*expType*expType
| Cast of string*expType
| Fun of string*expType list (*appel de la fonction/Class*)
| Call of string*expType
| Cast of string*expType
| Return of expType
| None (*A voir si on garde ou pas, chances que non*)

(* Modifications *)

type decl = {
    lhs: string;
    typ: defType;
    isVar: bool;
    rhs: expType option;(*attention !!!! optionnel ou potentiellement Null*)
  }

type paramDecl = decl list

type funDecl={
  nom : string;
  para: paramDecl;
  typ : defType option; (*type de retour*) (*attention !!!! optionnel*)
  (*argType : expType list option; liste d'argument du constructeur parent*)
  bloc : decl list*expType;
  (* est ce qu'on ajoute un bool pour savoir si cons*)
}
type classBloc ={
  dec : decl list;
  cons : funDecl;
  fon : funDecl list;

}

type classDecl = {
  nom : string;
  para : paramDecl;
  ext : defType option;(*attention !!! optionnel*)
  clb : classBloc;
  }

type objetDecl = {
  nom : string;
  dec : decl list;
  fon : funDecl list;
}

type classObjDecl =
  Class of classDecl
  |Objet of objetDecl

type prog = classObjDecl list*decl list*expType
