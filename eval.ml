open Ast

(*
---Verificateur Contextuel---
*)

(*Pour declarer variable "globale" et eviter de le redonner a chaque parametre*)
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

(*pour debug (et c'est hallucinant que ocaml l'ai pas) *)
let print_bool b=
  Printf.printf "%B" b
  ;;

let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let ajouterClassesDefauts ld= ()
(*TODO on ajoute les classes String et Integer *)
;;

let ajouterAttr (a : decl) r =
  if (Hashtbl.mem r a.lhs && a.isVar)
  then
  (*failwith ("an attribut with name "^a.lhs^" already exist (probably in parent)")*)
  Hashtbl.add r a.lhs a
  else
    Hashtbl.add r a.lhs a
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
let remplirClasse2 (x: classDecl) (parent: classHash option)= 
  if (Hashtbl.mem !table.objet x.nom || Hashtbl.mem !table.objet x.nom) 
  then failwith ("error a class or objet with name "^x.nom^" already exist") 
  else
    let atable = match parent with
    |Some p -> Hashtbl.copy p.attr
    |None -> Hashtbl.create 50
    in
    let mtable = match parent with
    | Some p -> Hashtbl.copy p.meth
    |None -> Hashtbl.create 50 
    in

    let co = ref None in
    List.iter (fun d -> ajouterAttr d atable) x.para;
    List.iter (
      fun d -> match d with
      | Fun a -> ajouterMeth a mtable 
      | Con a -> if (!co <> None) then failwith ("error class "^x.nom^" has more than 1 constructor") else co := Some a
      | Att a -> if (not a.isVar) then failwith ("error attribut "^a.lhs^" is not a var") else ajouterAttr a atable
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
let rec remplirClasse1 (nom : string) temp=

  (*si elle a deja ete ajoute aux donnes de classes *)
  if (Hashtbl.mem !table.classe nom) 
  then Hashtbl.find !table.classe nom
  else
  begin
    let x = Hashtbl.find temp nom in
    match x.ext with
    |Some a -> remplirClasse2 x (Some (remplirClasse1 a temp))
    |None -> remplirClasse2 (Hashtbl.find temp nom) None
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
  |Class x -> remplirClasse1 x.nom temp; ()
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

(*On test la methode d'une classe *)
let checkMethodClasse meth vars data=

;;

(*On teste toutes les methodes d'une classe *)
let checkMethodsClasse c =
  Hashtbl.iter (fun a d -> checkMethodClasse d (Hashtbl.copy c.attr) c.data ) c.meth
;;

let eval ld e =

  let tmp = Hashtbl.create 50 in
  List.iter (fun d ->   match d with
  Class x -> Hashtbl.add tmp x.nom x
  |Objet x -> ()) ld;

  (*on ajoute les classes Integer et String*)
  ajouterClassesDefauts ld;

  (*on verifie les extends avant d'ajouter les classes (sinon erreur)*)
  Hashtbl.iter (fun a d -> testExtends d tmp) tmp;

  (*on rempli les hashtables et array*)
  List.iter (fun d -> remplirTableCO d tmp) ld;

  (*On verifie toutes les fonctions*)
  Hashtbl.iter (fun a d -> checkMethodClasse d) !table.classe

  (* TODO :
  check instruction types
  check methodes sens
  *)

  print_newline ();
  print_string "Verificated with success";
  print_newline ();

;;
