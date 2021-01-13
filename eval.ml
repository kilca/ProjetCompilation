open Ast
open Primitives
(*
---Verificateur Contextuel---
*)

(*Pour declarer variable "globale" et eviter de le redonner a chaque parametre*)

(*
string1 : nom de la fonction
string2 list: nom des types de la fonction
*)
type methParam = string*string list;;

type classHash = {
  data : Ast.classDecl;
  attr : ((string, Ast.decl) Hashtbl.t);
  meth : ((methParam, Ast.funDecl) Hashtbl.t);
  cons : Ast.consDecl
};;

type objetHash = {
  data : Ast.objetDecl;
  attr : ((string, Ast.decl) Hashtbl.t);
  meth : ((methParam, Ast.funDecl) Hashtbl.t);
};;

type tableCO = 
  { 
    mutable classe : ((string, classHash) Hashtbl.t);
    mutable objet : ((string, objetHash) Hashtbl.t) 
  };;

type choixBloc = Classe | Objet | Main;;

(*pour debug (et c'est hallucinant que ocaml l'ai pas) *)
let print_bool b=
  Printf.printf "%B" b
  ;;

let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let ajouterClassesDefauts ld=
  [Ast.Class(Primitives.stringc);Ast.Class(Primitives.integerc)]@ld
;;

let ajouterAttr (a : decl) r (enTete : bool) =
  if (a.lhs = "this") then failwith "an attribut cannot be named this";
  if (enTete) then (*si dans parenthese de classe genre : Couleur(ICI)*)
  begin
    if (a.isVar) then Hashtbl.add r a.lhs a
  end
  else (*si dans attributs de classe genre : Couleur(){ICI}*)
  begin
    if (not a.isVar) then failwith ("an attribut must have var if not defined in parent : "^a.lhs)
    else Hashtbl.add r a.lhs a
  end
;;


let to_methParam (a : funDecl)=
  let s = ref [] in
  List.iter (fun (x : decl) -> s := x.typ :: !s) a.para;
  (a.nom,!s)
;;

(*Todo check double function type*)
let ajouterMeth (a : funDecl) r =

  if (Hashtbl.mem r (to_methParam a))
  then 
    begin
      if (a.over) 
      then Hashtbl.add r (to_methParam a) a
      else failwith "function already defined (missing override ?)"
    end
  else
    begin
      if (not a.over) then
        Hashtbl.add r (to_methParam a) a
      else
        failwith "function must be present in parent"
    end
  ;;


(* rempli la classe *)
let remplirClasse (x: classDecl) (parent: classHash option)= 
  if (Hashtbl.mem !table.objet x.nom || Hashtbl.mem !table.objet x.nom) 
  then failwith ("error a class or objet with name "^x.nom^" already exist") 
  else
    let atable = match parent with (*table des attributs *)
    |Some p -> Hashtbl.copy p.attr
    |None -> Hashtbl.create 50
    in
    let mtable = match parent with (*table des methodes *)
    | Some p -> Hashtbl.copy p.meth
    |None -> Hashtbl.create 50 
    in

    let co = ref None in (*pointeur du constructeur (type Option(Con))*)
    List.iter (fun d -> ajouterAttr d atable true) x.para;
    List.iter (
      fun d -> match d with
      | Fun a -> ajouterMeth a mtable 
      | Con a -> if (!co <> None) then failwith ("error class "^x.nom^" has more than 1 constructor") else co := Some a
      | Att a -> if (not a.isVar) then failwith ("error attribut "^a.lhs^" is not a var") else (ajouterAttr a atable false)
    ) x.cbl;
    let c = !co in
    match c with
    |None ->failwith ("error there is no constructor in "^x.nom)
    |Some sc -> Hashtbl.add !table.classe x.nom {data=x;attr=atable;meth=mtable;cons=sc};
                Hashtbl.find !table.classe x.nom

;;


(*on parcours jusqu'au classes parents *)
(*[!] BUG SI EXTENDS CIRCULAIRE *)
(*techniquement un fold mais trop dangereu a faire *)
let rec remplirAPartirClasseParent (nom : string)  temp=

  (*si elle a deja ete ajoute aux donnes de classes *)
  if (Hashtbl.mem !table.classe nom) 
  then Hashtbl.find !table.classe nom
  else
  begin
    let x = Hashtbl.find temp nom in
    match x.ext with
    |Some a -> remplirClasse x (Some (remplirAPartirClasseParent a temp))
    |None -> remplirClasse (Hashtbl.find temp nom) None
  end
;;

let remplirObjet (x : Ast.objetDecl)=
  if (Hashtbl.mem !table.objet x.nom || Hashtbl.mem !table.objet x.nom) 
  then failwith ("error a class or objet with name "^x.nom^" already exist")
  else
  let atable = Hashtbl.create 50 in
  let mtable = Hashtbl.create 50 in
  List.iter (fun a -> 
    match a with
    | Att e -> Hashtbl.add atable e.lhs e
    | Fun e -> ajouterMeth e mtable
    | Con e -> failwith "error constructor in object (??error should have happened in syntax)"
  ) x.cbl;
  Hashtbl.add !table.objet x.nom {data=x;attr=atable;meth=mtable}
;;

let remplirTableCO c temp=
  match c with
  |Class x -> begin
      try
      remplirAPartirClasseParent x.nom temp; ()
      with Stack_overflow -> failwith "Extends Erreur, Extends Circulaire ?"
    end
  |Objet x -> remplirObjet x;
;;

(*on vérifie si la classe extends existe et qu'elle n'extend pas elle meme *)
let testExtends (x : classDecl) tbl= 
    match x.ext with
    | Some a ->  
    if (not (Hashtbl.mem tbl a)) 
    then failwith ("error the class extended by "^x.nom^" doesn't exist")
    else if (x.nom = a)
    then failwith ("error the class : "^x.nom^" cannot extends itself")
    | None -> ()
;;

let is_first_uppercase (s : string) =
  if (s = "") then false
  else begin
    let c : char = s.[0] in
    (Char.code(c) >= Char.code('A') && Char.code(c) <= Char.code('Z'))
  end
;;

let expr_to_typestring (e : expType) (variables : ((string, Ast.decl) Hashtbl.t)) (lieu : string) =
  match e with
  Id (i : string) -> (Hashtbl.find variables i).typ 
  | ClassID (ci : string) -> (Hashtbl.find !table.objet ci).data.nom
  | Cste (c : constInt) -> "Integer"
  | CsteStr (c : constString) -> "String"
  | Plus (e1,e2) -> "Integer"
  | Minus (e1,e2) -> "Integer"
  | Times (e1,e2) -> "Integer"
  | Div (e1,e2) -> "Integer"
  | Concat (e1,e2) -> "String"
  | UMinus e1 -> "Integer"
  | UPlus e1 -> "Integer"
  | Comp (o,e1,e2) -> "Integer"
  | Cast (s,e) -> s
  | Selec (et,s) -> s (*TODO *)
  (*
  begin
    if (is_first_uppercase s) then
      begin
        let objet = (Hashtbl.find !table.objet ci) in
        let ex = Hashtbl.find objet.attr
      end
    else (Hashtbl.find variables i).typ 
    end
    *)
  | Call (e,s,el) -> s (*TODO *)
  (*
  begin
  end
  *)
  | Inst (s,el) -> s


;;

(*verifie que les parametre du constructeur appele est le bon *)
let checkParamSuperConstructeur (super : superO) (variables : ((string, Ast.decl) Hashtbl.t)) (nomClasse : string) =
  let param = super.para in
  let nom = super.ex in
  (*la verification du fait que le nom existe a deja ete fait implicitement 
  vu qu'il a verifie que le extend est bon et que super = extend
  *)
  let (constructorparent : consDecl) = (Hashtbl.find !table.classe nom).cons in

  if ((List.length constructorparent.para) <> (List.length param)) 
  then failwith ("error the number of arg of superconstructor called : "^nomClasse ^" doesn't match his class")
  else
  begin
    List.iter2 (fun (a : decl) (b : expType) -> 
    if (a.typ <> (expr_to_typestring b variables nomClasse)) then failwith ("error the type of argument of supercons doesnt match : "^nomClasse)
    ) constructorparent.para param
  end


;;

(*check si le constructeur de la classe a bien tout les memes parametres que la classe*)
let checkConstructeur (classeAttr : Ast.classDecl) (constr : Ast.consDecl) =
  if ((List.length classeAttr.para) <> (List.length constr.para)) 
  then failwith ("error the number of arg of constructor of "^classeAttr.nom ^" doesn't match his class")
  else
  begin
    List.iter2 (fun (a : decl) (b : decl) -> if ((a.typ <> b.typ) || (a.isVar <> b.isVar) || (a.lhs <> b.lhs))
    then failwith ("error the type of param of constructor of "^classeAttr.nom ^" doesn't match his class")
    ) classeAttr.para constr.para
  end
  ;
  if (constr.nom <> classeAttr.nom) then failwith "error the constructor must have the same name as the class (it shouldnt happen here)";
  match (constr.superrr,classeAttr.ext) with
  None,None -> ()
  |Some x, None -> failwith ("there must be a extend in the class if there is in the constructor in "^classeAttr.nom)
  |None, Some x -> failwith ("there must be a extend in the constructor if there is in the class in "^classeAttr.nom)
  |Some a, Some b ->
    let (nomSuper : string) = a.ex in
    let (nomExtend : string) = b in
    if (nomSuper <> nomExtend) then failwith ("super of constructor is different than the extend of the class : "^classeAttr.nom)
    else 
    checkParamSuperConstructeur a ((Hashtbl.find !table.classe constr.nom).attr) constr.nom
;;

let ajouterDeclarations (declarations : decl list) (variables : ((string, Ast.decl) Hashtbl.t)) =
  List.iter (fun x -> 
  if (x.lhs = "this") then failwith "error, can't create a variable with name this"
  else begin
    Hashtbl.add variables x.lhs x
  end
  ) declarations
;;

(*info method  : nomClasse*methParam*)
(*fonction qui check le bloc de soit une methode de soit le main *)
(*quelbloc : soit Main, soit Classe, soit Objet *)
let rec checkBloc (bloc: Ast.blocType) (infomethod : string*methParam) (variables : ((string, Ast.decl) Hashtbl.t)) (quelbloc : choixBloc) =


  (*Pour aider (attention marche pas si quelbloc est Main ou Objet)*)
  (*if quelbloc = Classe) then *)
  let (currentClass : classHash) = Hashtbl.find !table.classe (fst infomethod) in
  let (currentMethod : Ast.funDecl) = Hashtbl.find currentClass.meth (snd infomethod) in
  
  let variablesLocales = Hashtbl.create 50 in
  ajouterDeclarations (fst bloc) variablesLocales;(*on ajoute les declarations locales du bloc *)
  let instructions = snd bloc in
      (* Etapes pour prendre en compte la portee : *)
      (* -  On check d'abord si declaration dans variables locales, sinon dans variables et sinon erreur *)
      (*Techniquement il modifiera seulement si dans variables (mais on modifie pas en vc) *)
  let checkInstructions ins =
    match ins with
      Expr e -> ()
    | Bloc b -> 
      begin
        (*on ajoute les variables locales a une copie des variables du bloc du dessus *)
        let sousVariables = Hashtbl.copy variables in
        Hashtbl.iter (fun a b -> Hashtbl.add sousVariables a b) variablesLocales;
        checkBloc b infomethod variablesLocales quelbloc
      end
    | Return oe -> ()
    | Ite (e,it,ie) -> ()
    | Assign (el,er) -> 
      begin

      end
  in List.iter (fun x -> checkInstructions x) instructions
;;

(*On teste toutes les methodes d'une classe *)
let checkAllClassMethod (c :classHash) =

  (*a voir si c est une bonne chose *) 
  let (nomClasse : string) = c.data.nom in
  let thisdecl =
    {
    lhs = "this";
    typ= nomClasse; 
    isVar = false;(* a revoir *)
    rhs = None;
    } in
  let remplirVariablesEtCallCheckBloc (methpar : methParam) (fdec : Ast.funDecl) =
    let variablesClassetParam = Hashtbl.copy c.attr in
    Hashtbl.add variablesClassetParam "this" thisdecl;
    List.iter (fun dec -> 
    begin
      if (dec.isVar && not (Hashtbl.mem c.attr dec.lhs)) 
      then failwith ("a parameter with var is not an attribute in : "^ (fst methpar)^ " at "^ nomClasse)
      else Hashtbl.add variablesClassetParam dec.lhs dec
    end
    ) fdec.para;
    checkBloc fdec.corp (nomClasse,methpar) variablesClassetParam Classe
  in Hashtbl.iter (fun methpara fdec -> remplirVariablesEtCallCheckBloc methpara fdec) c.meth
;;


let eval ld e =

  (*on ajoute les classes Integer et String*)
  let nouvld = (ajouterClassesDefauts ld) in

  let tmp = Hashtbl.create 50 in
  List.iter (fun d ->   match d with
  Class x -> Hashtbl.add tmp x.nom x
  |Objet x -> ()) nouvld;

  (*on verifie les extends avant d'ajouter les classes (sinon erreur)*)
  Hashtbl.iter (fun a d -> testExtends d tmp) tmp;

  (*on rempli les hashtables et array*)
  List.iter (fun d -> remplirTableCO d tmp) nouvld;

  (*on verifie tous les constructeur *)
  Hashtbl.iter (fun a (d : classHash) -> checkConstructeur d.data d.cons) !table.classe;

  (*On verifie toutes les fonctions des classes*)
  Hashtbl.iter (fun a d -> checkAllClassMethod d) !table.classe;
  (*TODO : check Constructor Method *)

  (*TODO checkAllObjetMethod *)

  (*TODO checkMain *)

  print_newline ();
  print_string "Verificated with success";
  print_newline ();

;;
