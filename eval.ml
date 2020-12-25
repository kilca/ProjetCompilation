open Ast

(*
---Verificateur Contextuel---
*)

(*Pour declarer variable "globale" et eviter de le redonner a chaque parametre*)

(*meth list car objet a deja meth list *)
(*att list car plus facile a parcourir *)

type classHash = {
  data : Ast.classDecl;
  attr : Ast.decl list;
  meth : Ast.funDecl list;
  cons : Ast.consDecl option(*jamais None mais bon .. *)
};;

type objetHash = {
  data : Ast.objetDecl;
  attr : Ast.decl list;
  meth : Ast.funDecl list;
};;

type tableCO = 
  { 
    mutable classe : ((string, classHash) Hashtbl.t);
    mutable objet : ((string, objetHash) Hashtbl.t) 
  };;


let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let ajouterClassesDefauts ld= ()
(*on ajoute les classes String et Integer *)
;;

let ajouterAttr (a : decl) r =
  if (not (List.for_all (fun d -> (d.lhs <> a.lhs)) !r ) )
  then
  failwith ("an attribut with name "^a.lhs^" already exist")
  else
    r := a::!r;
;;

let remplirClasse (x : classDecl)= 
  if (Hashtbl.mem !table.objet x.nom || Hashtbl.mem !table.objet x.nom) 
  then failwith ("error a class or objet with name "^x.nom^" already exist") 
  else
    let att = ref [] in
    let met = ref [] in
    let co = ref None in
    List.iter (fun d -> ajouterAttr d att) x.para;
    List.iter (
      fun d -> match d with
      | Fun a -> met := a::!met 
      | Con a -> if (!co <> None) then failwith ("error class "^x.nom^" has more than 1 constructor") else co := Some a
      | Att a -> if (not a.isVar) then failwith ("error attribut "^a.lhs^" is not a var") else ajouterAttr a att
    ) x.cbl;
    let at = !att in
    let me = !met in
    let c = !co in
    if (!co = None) then failwith ("error there is no constructor in "^x.nom)
    else Hashtbl.add !table.classe x.nom {data=x;attr=at;meth=me;cons=c}

;;

let remplirObjet (x : Ast.objetDecl)=
  if (Hashtbl.mem !table.objet x.nom || Hashtbl.mem !table.objet x.nom) 
  then failwith ("error a class or objet with name "^x.nom^" already exist")
  else
  Hashtbl.add !table.objet x.nom {data=x;attr=x.dec;meth=x.fon}
;;

let remplirTableCO c =
  (*TODO REMPLIR METHODES PAR ORDRE DE PARENT *)
  (*TODO APPELER TESTEXTENDS ICI *)
  (*TODO AJOUTER ATTRIBUTS DE EXTENDED *)
  match c with
  Class x -> remplirClasse x
  |Objet x -> remplirObjet x
;;

(*todo check attributs heritage *)
let testExtends c=
  match c with
  Class x -> 
    begin
    match x.ext with
    | Some a ->  
    if (Hashtbl.mem !table.classe a) 
    then failwith ("error the class extended by"^x.nom^" doesn't exist")
    else if (x.nom = a)
    then failwith ("error a class :"^x.nom^" cannot extends themself")
    | None -> ()
    end
  |Objet x -> ()
;;

let eval ld e =

  ajouterClassesDefauts ld;
  (*on ne fait que remplir les hashtables et array (on verifie la base mais pas extends) *)
  List.iter (fun d -> remplirTableCO d) ld;

  (*on verifie les extends et on rempli encore*)
  List.iter (fun d -> testExtends d) ld
;;
