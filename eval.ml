open Ast
open Primitives
(*
---Verificateur Contextuel---
*)


(*
string1 : nom de la fonction
string2 list: nom des types de la fonction
*)

(*let (result : expType) = Cste(Int(0));;*)

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

(*Pour declarer variable "globale" et eviter de le redonner a chaque parametre*)
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

(*hashtable rendant facile l'acces a tous les attributs/variables *)
(*utilisation : !table.classe ou !table.objet *)
let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

(*on ajoute les classes des primitives (String et Integer) *)
let ajouterClassesDefauts ld=
  [Ast.Class(Primitives.stringc);Ast.Class(Primitives.integerc)]@ld
;;

(*fonction qui ajoute un attribut (utilise pour remplir la classe et objet *)
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

(*de funDecl a methParam *)
let to_methParam (a : funDecl)=
  let s = ref [] in
  List.iter (fun (x : decl) -> s := x.typ :: !s) a.para;
  (a.nom,!s)
;;

(*Todo check double function type (fait ?)*)
(* fonction qui permet d'ajouter une methode (utilise pour remplir la classe et objet*)
(* r: hashtble soit de classe soit de objet *)
let ajouterMeth (a : funDecl) r =

  if (Hashtbl.mem r (to_methParam a))
  then 
    begin
      if (a.over) 
      then Hashtbl.add r (to_methParam a) a
      else failwith ("function already defined (missing override ?) : "^a.nom)
    end
  else
    begin
      if (not a.over) then
        Hashtbl.add r (to_methParam a) a
      else
        failwith ("function with override must be present in parent : "^a.nom)
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
(*techniquement un fold mais trop risque a faire *)
let rec remplirAPartirClasseParent (nom : string)  temp=

  (*si elle a deja ete ajoute aux donnes de classes *)
  if (Hashtbl.mem !table.classe nom) 
  then Hashtbl.find !table.classe nom
  else
  begin
    let x = Hashtbl.find temp nom in
    match x.ext with
    |Some a -> remplirClasse x (Some (remplirAPartirClasseParent a temp));
    |None -> remplirClasse (Hashtbl.find temp nom) None;
  end
;;

(*on rempli !table.objet avec l'objet courant*)
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

(*on rempli !table.classe et !table.objet *)
let remplirTableCO c temp=
  match c with
  |Class x -> begin
      try
      (*on ignore le retour, et on rempli a partir de la classe superParent *)
      let _ = remplirAPartirClasseParent x.nom temp in
      ()
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


(*si une classe a le parent en classe paren (peut le cast )*)
let rec haveThisParent (nomclasse : string) (nomparent : string) =
  let (parent : string option) = (Hashtbl.find (!table.classe) nomclasse).data.ext in
  match parent with
  None -> false
  |Some x -> if (nomparent = x) then true else haveThisParent x nomparent
;;

(*converti un string en choixBloc *)
(*considere qu'il n'y a pas de doublon entre classe et objet *)
let string_to_choixbloc (s : string)=
  if (Hashtbl.mem !table.classe s) then Classe
  else if (Hashtbl.mem !table.objet s) then Objet
  else Main
;;

(*Verifie si une expression est logique et renvoie son type (en string)*)
(*e : expression a convertir et checker*)
(*variables : variables locales du bloc *)
(*lieu : endroit d'appel de expr_to_typestring (mettre "" si main) *)
let rec expr_to_typestring (e : expType) (variables : ((string, Ast.decl) Hashtbl.t)) (lieu : string) =
  
  let (ou : choixBloc)= string_to_choixbloc lieu in

  match e with
  Id (i : string) -> (*si l'expression est un id*)
      if (i = "this") then
      begin
        if (ou = Main) then failwith ("this ne peut pas etre appele dans le main :"^lieu)
        else lieu
      end
      else if (i = "super") then
      begin
        if (ou = Main || ou = Objet) then failwith ("super ne peut etre appelee que dans la classe:"^lieu)
        else begin
          let courant = Hashtbl.find !table.classe lieu in
          match courant.data.ext with (*match classe extended *)
          None ->  failwith ("la classe parente appelee par super n existe pas")
          | Some x -> x
        end
      end
      else if (not (Hashtbl.mem variables i)) then failwith ("la variable "^i^ " n'existe pas dans le bloc de : ("^lieu^"?)")
      else (Hashtbl.find variables i).typ
  | ClassID (ci : string) -> (*si l'expression est un objet*)
    if (not (Hashtbl.mem !table.objet ci)) then failwith ("l'objet appele n'existe pas")  
    else ci
  | Cste (c : constInt) -> "Integer" (*si l'expression est un int*)
  | CsteStr (c : constString) -> "String" (*si l'expression est un string*)
  | Plus (e1,e2) -> (*si l'expression est un +*)
    begin
      if (((expr_to_typestring e1 variables lieu) <> "Integer") 
      || ((expr_to_typestring e2 variables lieu) <> "Integer"))
      then failwith ("error the expression in plus must be integer in "^lieu)
      else "Integer"
    end
  | Minus (e1,e2) -> (*si l'expression est un -*)
    begin
      if (((expr_to_typestring e1 variables lieu) <> "Integer") 
      || ((expr_to_typestring e2 variables lieu) <> "Integer"))
      then failwith ("error the expression in minus must be integer in "^lieu)
      else "Integer"
    end
  | Times (e1,e2) -> (*si l'expression est un * *)
    begin
      if (((expr_to_typestring e1 variables lieu) <> "Integer") 
      || ((expr_to_typestring e2 variables lieu) <> "Integer"))
      then failwith ("error the expression in times must be integer in "^lieu)
      else "Integer"
    end
  | Div (e1,e2) -> (*si l'expression est un / *)
    begin
      if (((expr_to_typestring e1 variables lieu) <> "Integer") 
      || ((expr_to_typestring e2 variables lieu) <> "Integer"))
      then failwith ("error the expression in div must be integer in "^lieu)
      else "Integer"
    end
  | Concat (e1,e2) ->(*si l'expression est un & *)
    begin
      if (((expr_to_typestring e1 variables lieu) <> "String") 
      || ((expr_to_typestring e2 variables lieu) <> "String"))
      then failwith ("error the expression in times must be String in "^lieu)
      else "String"
    end
  | UMinus e1 ->(*si l'expression est un -a *) 
    begin
      if ((expr_to_typestring e1 variables lieu) <> "Integer") 
      then failwith ("error the expression in times must be Integer in "^lieu)
      else "Integer"
    end
  | UPlus e1 -> (*si l'expression est un +a *)
    begin
      if ((expr_to_typestring e1 variables lieu) <> "String")
      then failwith ("error the expression with plus before must be Integer in "^lieu)
      else "Integer"
    end
  | Comp (o,e1,e2) -> (*si l'expression est un a==a*)
    if (((expr_to_typestring e1 variables lieu) <> "Integer") 
    || ((expr_to_typestring e2 variables lieu) <> "Integer"))
    then failwith ("error the expression in comparison must be Integer in "^lieu)
    else "Integer"
  | Cast (s,e) -> (*si l'expression est un (a as C)*)
    begin
      let typee = expr_to_typestring e variables lieu in
      if ( (not (haveThisParent typee s)) || (typee<>s)) then failwith ("error expression"^typee^" cannot be cast in "^s^" at :"^lieu)
      else typee
    end
  | Selec (e,s) -> (*si l'expression est un a.b*)
  begin
  let (sexpr : string) = expr_to_typestring e variables lieu in
  let (ousexpr : choixBloc) = string_to_choixbloc sexpr in
  match ousexpr with 
    | Classe ->
        let classe = (Hashtbl.find !table.classe sexpr) in
        if (Hashtbl.mem classe.attr s) then
          (Hashtbl.find classe.attr s).typ
        else failwith ("the attribute : "^s^" doesn't exist in "^classe.data.nom)
    | Objet ->
        let objet = (Hashtbl.find !table.objet sexpr) in
        if (Hashtbl.mem objet.attr s) then
          (Hashtbl.find objet.attr s).typ
        else failwith ("the attribute : "^s^" doesn't exist in "^objet.data.nom)
    | Main -> failwith ("cannot call an attribute from main (how did this happen ?)")
    end
  | Call (e,s,el) -> (*si l'expression est un a.f()*)
  begin
    let (sexpr : string) = expr_to_typestring e variables lieu in
    let (ousexpr : choixBloc) = string_to_choixbloc sexpr in
    let (params : methParam) = (s,List.map (fun x -> expr_to_typestring x variables lieu) el) in
    match ousexpr with 
    | Classe ->
        let classe = (Hashtbl.find !table.classe sexpr) in
        if (Hashtbl.mem classe.meth params) then
        begin
          let (nomretour : string option)=(Hashtbl.find classe.meth params).typ in
          match nomretour with
          | None -> ""
          | Some x -> x
        end
        else failwith ("the function : "^s^" doesn't exist in "^classe.data.nom)
    | Objet ->
        let objet = (Hashtbl.find !table.objet sexpr) in
        if (Hashtbl.mem objet.meth params) then
        begin
          let (nomretour : string option)=(Hashtbl.find objet.meth params).typ in
          match nomretour with
          | None -> ""
          | Some x -> x
        end
        else failwith ("the attribute : "^s^" doesn't exist in "^objet.data.nom)
    | Main -> failwith ("cannot call a function from main (how did this happen ?)")
  end
  | Inst (s,el) ->  (*si l'expression est un new C()*)
  begin
    if (not (Hashtbl.mem !table.classe s)) then failwith ("la classe instantiee n'existe pas : "^s)
  else 
    begin
      let attrEnTete = (Hashtbl.find !table.classe s).attr  in (*on recupere la liste des attributs en en tete de classe*)  
      (* attrEnTete est pour le moment une hashtabme ùais il faut que ça soit une liste *)   
      let plstAttrEnTete = ref [] in
      Hashtbl.iter (fun x y -> plstAttrEnTete:= y::(!plstAttrEnTete)) attrEnTete;
      let lstAttrEnTete = !plstAttrEnTete in
      if (Hashtbl.length attrEnTete) <> (List.length el) then 
        begin
          failwith ("mauvais nombre de parametre") (*on compare les tailles qui doivent etre egale*)
        end
        else
        begin (*on compare si les attributs entre entete et instanciation*)
          List.iter2 (fun (a : expType) (b : decl) ->
           if ( (expr_to_typestring a variables lieu) <> b.typ (*???*)) then failwith ("type de parametre non correspondant")
          ) el lstAttrEnTete;
           ""
        end
    end
  end
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
  begin (*techniquement meme verification que serait dans checkbloc *)
    List.iter2 (fun (a : decl) (b : expType) -> 
    if (a.typ <> (expr_to_typestring b variables nomClasse)) then failwith ("error the type of argument of supercons doesnt match : "^nomClasse)
    ) constructorparent.para param
  end
;;

(*Todo check param var correcte ? (ou alors deja fait dans en tete classe ?) *)
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
  
  (*on compare l'extend de la classe et le super du constructeur *)
  match (constr.superrr,classeAttr.ext) with
  None,None -> () (*si le constructeur n'a pas de parent et class n'a pas de parent *)
  |Some x, None -> failwith ("there must be an extend in the class if there is in the constructor in "^classeAttr.nom)
  |None, Some x -> failwith ("there must be an extend in the constructor if there is in the class in "^classeAttr.nom)
  |Some a, Some b ->(*si le constructeur a un parent et class a un parent *)
    let (nomSuper : string) = a.ex in
    let (nomExtend : string) = b in
    if (nomSuper <> nomExtend) then failwith ("super of constructor is different than the extend of the class : "^classeAttr.nom)
    else 
    begin
    let variables = (Hashtbl.find !table.classe constr.nom).attr in
    List.iter (fun (x : Ast.decl) -> if (not x.isVar) then Hashtbl.add variables x.lhs x) constr.para;

    checkParamSuperConstructeur a variables constr.nom
    end
;;

(*ajouter la liste de declaration dans la hashtable variables (utilise dans checkBloc)*)
(*TODO CHECK INIT *)
let ajouterDeclarations (declarations : decl list) (variables : ((string, Ast.decl) Hashtbl.t)) =
  List.iter (fun x -> 
  if (x.lhs = "this") then failwith "error, can't create a variable with name this"
  else if (x.lhs = "super") then failwith "error, can't create a variable with name super"
  else
    Hashtbl.add variables x.lhs x
  ) declarations
;;

let checkAssignDeclaration (dec : Ast.decl) variables nomCO =
  match dec.rhs with
  | None -> ()
  | Some x -> 
  let nomTypeDroite = expr_to_typestring x variables nomCO in
  if (nomTypeDroite <> dec.typ) then failwith ("erreur mauvais type d'init de variable : "^dec.lhs^" dans : "^nomCO)
;;

(*fonction qui check le bloc de soit une methode de soit le main *)
(*info method  : nomClasse*methParam*)
(*quelbloc : soit Main, soit Classe, soit Objet *)
let rec checkBloc (bloc: Ast.blocType) (infomethod : string*methParam) (variables : ((string, Ast.decl) Hashtbl.t)) (quelbloc : choixBloc) =


  (*Pour aider (attention marche pas si quelbloc est Main ou Objet)*)
  (*if quelbloc = Classe) then *)
  (*
  let (currentClass : classHash) = Hashtbl.find !table.classe (fst infomethod) in
  let (currentMethod : Ast.funDecl) = Hashtbl.find currentClass.meth (snd infomethod) in
  *)

  let variablesLocales = Hashtbl.create 50 in
  ajouterDeclarations (fst bloc) variablesLocales;(*on ajoute les declarations locales du bloc *)
  let instructions = snd bloc in
      (* Etapes pour prendre en compte la portee : *)
      (* -  On check d'abord si declaration dans variables locales, sinon dans variables et sinon erreur *)
      (*Techniquement il modifiera seulement si dans variables (mais on modifie pas en vc) *)
  let rec checkInstructions ins =
    match ins with
      Expr e -> 
          (*faire exptypestring*)
        begin
          match quelbloc with
            | Classe -> expr_to_typestring e variables "Classe";
            | Objet -> expr_to_typestring e variables "Object";
            | Main -> expr_to_typestring e variables "Main";
                
        end;
        ();
        (*
        if Hashtbl.mem variablesLocales e then true
          else if Hashtbl.mem variables e then true 
            else failwith "not a good instruction"
        *)
    | Bloc b -> 
      begin
        (*on ajoute les variables locales a une copie des variables du bloc du dessus *)
        let sousVariables = Hashtbl.copy variables in
        Hashtbl.iter (fun a b -> Hashtbl.add sousVariables a b) variablesLocales;
        checkBloc b infomethod variablesLocales quelbloc
      end
    | Return oe -> 
        begin
        match quelbloc with 
        | Classe -> ()
        (*(Hashtbl.find (!table.classe) (snd infomethod)).typ*)
                (*
                let inf = Hashtbl.find (!table.classe) (fst infomethod) in
                    let ht = Hashtbl.find (inf.meth) (snd infomethod) in
              if ht =  expr_to_typestring oe variables "Classe" then ()
                      else failwith "trying to return a different type than the type method"
                    *)
            (*pour None -> renvoie dernière instanciation*)

            (*
            match oe with 
            | None -> ()
            
              if (Hashtbl.find (!table.classe) (snd infomethod)).typ = None then ()
                        else failwith "trying to return a different type than the type method"
            *)
            (*if (Hashtbl.find (!table.classe) (snd infomethod)).typ =  expr_to_typestring e variables "Classe" then ()
                      else failwith "trying to return a different type than the type method"
            *)
        | Objet -> ()
          (*
            let inf = Hashtbl.find (!table.objet) (fst infomethod) in
                    let ht = Hashtbl.find (inf.meth) (snd infomethod) in
              if ht =  expr_to_typestring oe variables "Classe" then ()
                      else failwith "trying to return a different type than the type method"
          *)
        | Main -> ()
        
      end
    
        (*match quelbloc with *)

        (*check que oe est de même type que type de retour de la méthode*)
        (*
        | Classe -> 
              if expr_to_typestring oe variables "Classe" = expr_to_typestring er variables "Classe" then ()
                else failwith "trying to return another type than the method type name" 
          | Objet -> 
              
              if expr_to_typestring el variables "Objet" = expr_to_typestring er variables "Objet" then ()
                else failwith "trying to return another type than the method type name"
              
          | Main -> 
              
              if expr_to_typestring el variables "Main" = expr_to_typestring er variables "Main" then ()
                else failwith "trying to return another type than the method type name"
          *)

    | Ite (e,it,ie) -> checkInstructions it;checkInstructions ie;

      begin
        match quelbloc with
          | Classe -> 
              if expr_to_typestring e variables "Classe" = "Integer" then ()
              else failwith "type integer is required" 
            
          | Objet -> 
              if expr_to_typestring e variables "Object" = "Integer" then ()
              else failwith "type integer is required" 
              
          | Main -> 
               if expr_to_typestring e variables "Main" = "Integer" then ()
              else failwith "type integer is required" 
              
      end
        
        
        (*| _ -> "you have to put an integer on the condition"*)
    | Assign (el,er) -> 
      begin
        (*verifier que el n'est pas une cste *)
        match el with
        |Cste(el) -> failwith "trying to assign a const"
        |CsteStr(el) -> failwith "trying to assign a const"
        |_ -> 
          match quelbloc with
          | Classe -> 
              if expr_to_typestring el variables "Classe" = expr_to_typestring er variables "Classe" then ()
                else failwith "trying to assigne 2 different types" ; 
              if Hashtbl.mem !table.objet (expr_to_typestring el variables "Classe") then failwith "trying to assigne an object"
                else ()
          | Objet -> 
              
              if expr_to_typestring el variables "Objet" = expr_to_typestring er variables "Objet" then ()
                else failwith "trying to assigne 2 different types" ;
              if Hashtbl.mem !table.objet (expr_to_typestring el variables "Objet") then failwith "trying to assigne an object"
                else ()
              
          | Main -> 
              
              if expr_to_typestring el variables "Main" = expr_to_typestring er variables "Main" then ()
                else failwith "trying to assigne 2 different types" ;
              if Hashtbl.mem !table.objet (expr_to_typestring el variables "Main") then failwith "trying to assigne an object"
                else ()
              
      
      (*verifier que expr_to_typestring de gauche est meme que celui de droite *)
      end
  in List.iter (fun x -> checkInstructions x) instructions
;;

(*On teste toutes les methodes d'une classe *)
(*c : classeHash ou objetHash *)
(*typ : Classe ou Objet *)
let checkAllMethodAndAttributs (nom : string) (attr) (funs) (super: string) (typ : choixBloc)=

  (*fonction qui ajoute this*)
  let ajouterthis varcparam=
    let thisdecl =
      {
      lhs = "this";
      typ= nom; 
      isVar = false;(* a revoir *)
      rhs = None;
      } in 
      Hashtbl.add varcparam "this" thisdecl; (*on ajoute this aux variables *)
    in
  (*fonction qui ajoute super *)
  let ajoutersuper varcparam= 
      let superdecl =
        {
        lhs = "super";
        typ= super; 
        isVar = false;(* a revoir *)
        rhs = None;
        } in 
      Hashtbl.add varcparam "super" superdecl; (*on ajoute super aux variables *)
  in

  (*utilisee que pour les methodes *)
  let remplirVariablesEtCallCheckBloc variables (methpar : methParam) (fdec : Ast.funDecl) =

    (*on verifie l'en tete de la fonction et on rempli*)
    List.iter (fun dec -> 
    begin
      if (dec.isVar && not (Hashtbl.mem attr dec.lhs)) 
      then failwith ("a parameter with var is not an attribute in : "^ (fst methpar)^ " at "^ nom)
      else Hashtbl.add variables dec.lhs dec
    end
    ) fdec.para;
    (*on verifie le bloc de la fonction *)
    checkBloc fdec.corp (nom,methpar) variables typ
  in 

  let variables2 = Hashtbl.copy attr in
  (*on ajoute super et this aux varibles *)

  ajouterthis variables2;
  if (super <> "") then ajoutersuper variables2;


  Hashtbl.iter (fun methpara fdec -> remplirVariablesEtCallCheckBloc variables2 methpara fdec) funs; (*on check le bloc des fonctions *)
  Hashtbl.iter (fun s dec -> checkAssignDeclaration dec variables2 nom) attr (*on check les init des variables *)
;;

let check_Main (b: Ast.blocType) =
  (*
  let variables = Hashtbl.create 50 in
  List.iter (fun (x : Ast.decl) ->
    if (Hashtbl.mem variables x.lhs) then print_string ("Warning : la variable : "^x.lhs^" existe deja dans main")
    (*check this ou super ? *)
    else Hashtbl.add variables x.lhs x
  ) (fst b);
  checkBloc (snd b) variables ("",[]) Main
  *)
  let truc = Hashtbl.create 50 in
  checkBloc b ("",("",[])) truc Main
;;

(*on check si doublon de nom de classe/objet *)
(*ld list de classe/objet *)
let check_ClasseObjet_duplicat ld =
  let rec check tabl ld=
    match ld with
    | [] -> ()
    | x::s ->
    begin match x with
      | Class c -> if (Hashtbl.mem tabl c.nom) then failwith ("erreur doublon de classe/objet : "^c.nom)
                  else Hashtbl.add tabl c.nom "useless"; check tabl s
      | Objet o -> if (Hashtbl.mem tabl o.nom) then failwith ("erreur doublon de classe/objet : "^o.nom)
                  else Hashtbl.add tabl o.nom "useless"; check tabl s
    end
  in check (Hashtbl.create 50) ld
;;

(*on check les doublons d'attributs *)
(*en tete : list de declaration de l'en tete (vide pour Object) *)
(*corp : liste de declaration dans le bloc *)
let check_Attribut_duplicat (nom : string) (enTete : Ast.decl list) (corp : Ast.decl list)=
  List.iter (fun x -> if (not x.isVar) then failwith ("error an attribut in bloc of class/object must have var :"^x.lhs^" , "^nom)) corp;
  let rec check tabl ld=
    match ld with
    | [] -> ()
    | x::s ->
    begin
      if (Hashtbl.mem tabl x.lhs) then failwith ("erreur doublon d'attribut : "^x.lhs^", dans "^nom)
      else Hashtbl.add tabl x.lhs "useless"; check tabl s
    end
  in check (Hashtbl.create 50) (enTete@corp)
;;

let call_checkAttrDuplicat (a :classObjDecl)=

  match a with
  |Class c -> 
  begin
    let enTete = c.para in
    let corp = ref [] in
    (*on ajoute tous les attributs declares dans le bloc dans corp *)
    List.iter (fun cbl ->
    match cbl with
    | Att a -> corp := a::!corp; 
    |_ -> ()
    ) c.cbl;
    check_Attribut_duplicat c.nom enTete !corp
  end
  |Objet c ->
  begin
    let enTete = [] in
    let corp = ref [] in
    (*on ajoute tous les attributs declares dans le bloc dans corp *)
    List.iter (fun cbl ->
    match cbl with
    | Att a -> corp := a::!corp; 
    |_ -> ()
    ) c.cbl;
    check_Attribut_duplicat c.nom enTete !corp
  end 

;; 

(*verifie l'entierete de l'ast (appele par main) *)
let eval ld e =

  (*on ajoute les classes Integer et String*)
  let nouvld = (ajouterClassesDefauts ld) in

  (*on check si duplicat de nom de classe/objet *)
  check_ClasseObjet_duplicat nouvld;(*verifiee aussi lors de l'ajout *)
  (*on check si duplicat de nom d'attributs dans classe/objet *)
  List.iter (fun x -> call_checkAttrDuplicat x) nouvld;

  let tmp = Hashtbl.create 50 in (*hash temporaire *)
  List.iter (fun d ->   match d with
  Class x -> Hashtbl.add tmp x.nom x
  |Objet x -> ()) nouvld;

  (*on verifie les extends avant d'ajouter les classes (sinon erreur)*)
  Hashtbl.iter (fun a d -> testExtends d tmp) tmp;

  (*on rempli les hashtables et array*)
  List.iter (fun d -> remplirTableCO d tmp) nouvld;

  (*on verifie tous les constructeur *)
  Hashtbl.iter (fun a (d : classHash) -> checkConstructeur d.data d.cons) !table.classe;

  (*On verifie toutes les fonctions et attributs des classes*)
  Hashtbl.iter (fun a (d : classHash) ->
  let (super : string) = (match d.data.ext with | None -> ""; | Some x -> x;) in
  checkAllMethodAndAttributs (d.data.nom) (d.attr) (d.meth) super Classe
  ) !table.classe;

  (*On verifie toutes les fonctions et attributs des objets*)
  
  Hashtbl.iter (fun a (d : objetHash) ->
  checkAllMethodAndAttributs (d.data.nom) (d.attr) (d.meth) "" Objet
   ) !table.objet;
  
  (*Verifie le main *)
  check_Main e;

  print_newline ();
  print_string "Verificated with success";
  print_newline ();

;;
