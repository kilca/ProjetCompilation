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
| Fun of string*expType list (*appel de la fonction/Class*)
| NULL (*A voir si on garde ou pas*)

(* Modifications *)

type decl = {
    lhs: string;
    typ: defType;
    rhs: expType;(*attention !!!! optionnel ou potentiellement Null*)
  }

type paramDecl = decl list

type funDecl={
  nom : string;
  para: paramDecl;
  typ : string; (*type de retour*) (*attention !!!! optionnel*)
  bloc : decl list*expType;
}

type classDecl = {
  nom : string;
  para : paramDecl;
  ext : classDecl;(*attention !!! optionnel*)
  dec : decl list;
  fon : funDecl list;
  }

type objetDecl = {
  dec : decl list;
  fon : funDecl list;
}

type classObjDecl =
  CLASS of classDecl
  |OBJET of objetDecl

type prog = classObjDecl list*decl list*expType
