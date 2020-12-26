open Ast

(*
---Verificateur Contextuel---
*)

(*TODO CHANGER LES LIST EN HASHTBL*)

(*Pour declarer variable "globale" et eviter de le redonner a chaque parametre*)
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

(*pour debug (et c'est hallucinant que ocaml l'ai pas) *)
let print_bool b=
  Printf.printf "%B" b
  ;;

let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let ajouterClassesDefauts ld= ()
(*TODO on ajoute les classes String et Integer *)
;;

let ajouterAttr (a : decl) r =
  if (not (List.for_all (fun d -> (d.lhs <> a.lhs)) !r ) )
  then
  failwith ("an attribut with name "^a.lhs^" already exist")
  else
    r := a::!r;
;;

(* rempli la classe *)
(*TODO PRENDRE EN COMPTE LA CLASSE PARENT EN AJOUTANT ATTRIBUTS (APRES AVOIR CHANGE EN HASHTBL)*)
let remplirClasse2 (x: classDecl) (parent: classHash option)= 
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
    else 
    begin 
      Hashtbl.add !table.classe x.nom {data=x;attr=at;meth=me;cons=c};
      Hashtbl.find !table.classe x.nom
    end

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
  Hashtbl.add !table.objet x.nom {data=x;attr=x.dec;meth=x.fon}
;;

let remplirTableCO c temp=
  match c with
  |Class x -> remplirClasse1 x.nom temp; ()
  |Objet x -> remplirObjet x;
;;

let testExtends (x : classDecl) tbl= 
    match x.ext with
    | Some a ->  
    if (not (Hashtbl.mem tbl a)) 
    then failwith ("error the class extended by"^x.nom^" doesn't exist")
    else if (x.nom = a)
    then failwith ("error a class :"^x.nom^" cannot extends themself")
    | None -> ()
;;

let eval ld e =

  let tmp = Hashtbl.create 50 in
  List.iter (fun d ->   match d with
  Class x -> Hashtbl.add tmp x.nom x
  |Objet x -> ()) ld;


  ajouterClassesDefauts ld;

  (*on verifie les extends avant d'ajouter les classes (sinon erreur)*)
  Hashtbl.iter (fun a d -> testExtends d tmp) tmp;

  (*on rempli les hashtables et array*)
  List.iter (fun d -> remplirTableCO d tmp) ld;

  print_newline ();
  print_string "Verificated with success";
  print_newline ();

;;
