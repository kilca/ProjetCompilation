open Ast

(* --------------- Types et fonctions utiles ------------------------- *)


type methParam = string*string list;;

(*peut etre rajouter attributs et meth comme dans eval*)
type classCode = {
  data : Ast.classDecl;
  attrIndex : ((string, int) Hashtbl.t);
  methEti : ((methParam, string) Hashtbl.t);
  (*ajouter constructeur *)
};; 
type objectCode = {
  data : Ast.objetDecl;
  attrIndex : ((string, int) Hashtbl.t);
  methEti : ((methParam, string) Hashtbl.t);
};; 

type tableCO = 
  { 
    mutable classe : ((string, classCode) Hashtbl.t);
    mutable objet : ((string, objectCode) Hashtbl.t) 
  };;

let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let cptEtiITE = ref 0;; (* compteur: Instruction globale *)
let cptEtiMeth = ref 0;; (*compteur : Declaration globale *)

let makeEtiMethod nomMethode= (* generateur d'etiquettes fraiches pour methodes *)
  let v = !cptEtiMeth in
  let sv = string_of_int v in
  cptEtiMeth := v + 1;
  "Method."^sv^ " --"^nomMethode
;;

let makeEtiITE () = (* generateur d'etiquettes fraiches pour ITE *)
  let v = ! cptEtiITE in
  let sv = string_of_int v in
  cptEtiITE := v + 1;
  ("ELSE"^sv, "FINIF"^sv)  (* ^ est l'operateur de concatenation de strings *)
;;

(*TODO : donner le type de l'expression*)
let expr_to_string e = ""
;;

(*[!] Todo : prendre en compte l'heritage *)
(*trouve l'etiquette a partir de la methode *)
let find_eti_methode nom nomMethode parametres =

  let paramTypes = List.map (fun x -> expr_to_string x) parametres in
  if (Hashtbl.mem !table.classe nom) then
  begin
    let (cCode : classCode) = Hashtbl.find !table.classe nom in
    Hashtbl.find cCode.methEti (nomMethode,paramTypes)
  end
  else
  begin
    let (oCode : objectCode) = Hashtbl.find !table.objet nom in
    Hashtbl.find oCode.methEti (nomMethode,paramTypes)
  end
;;

let remplirClassesObjet co =
  match co with
  | Objet o ->
          Hashtbl.add !table.objet 
            o.nom 
            {
              data = o;
              attrIndex = Hashtbl.create 50;
              methEti = Hashtbl.create 50;
            }
  | Class c ->
            Hashtbl.add !table.classe 
            c.nom 
            {
              data = c;
              attrIndex = Hashtbl.create 50;
              methEti = Hashtbl.create 50;
            }


;;

(*----------------------  Corp Generation Code ------------------------------ *)


(* obj : object, env : environment (structure abstraite), chan : string buffer *)
let rec compileObject obj env chan =
  output_string chan "-- compileObject\n";
  


(* cls : class, env : environment (structure abstraite), chan : string buffer *)
and compileClass cls env chan =
  output_string chan "-- compileClass\n";
  (*on appelera makeEtiMeth*)


(* exp : expType *)
and compileExpr exp env chan  =
(* output_string chan "\t\t-- compileExpr\n"; *)
  match exp with
    Id s -> output_string chan "\t\t-- Id\n";
            env
  | ClassID s -> 
              output_string chan "\t\t-- ClassID\n";
              env
  | Cste i ->  
              output_string chan "PUSHI "; output_string chan (string_of_int i);
              output_string chan "\n";
              env
  | CsteStr s ->
              output_string chan "PUSHS "; output_string chan s;
              output_string chan "\n";
              env
  | Plus (e1, e2) -> compileExpr e1 env chan; 
                    compileExpr e2 env chan; 
                    output_string chan "ADD\n";
                    env
  | Minus (e1, e2) -> 
                    compileExpr e1 env chan; 
                    compileExpr e2 env chan; 
                    output_string chan "SUB\n";
                    env
  | Times (e1, e2) -> 
                  compileExpr e1 env chan; 
                  compileExpr e2 env chan; 
                  output_string chan "MUL\n";
                  env
  | Div (e1, e2) ->
                  compileExpr e1 env chan; 
                  compileExpr e2 env chan; 
                  output_string chan "DIV\n";
                  env
  | Concat (e1, e2) ->
                  compileExpr e1 env chan; 
                  compileExpr e2 env chan; 
                  output_string chan "CONCAT\n";
                  env
  | UMinus e ->
                output_string chan "PUSHI 0\n";
                compileExpr e env chan;
                output_string chan "SUB\n";
                env
  | UPlus e ->
                compileExpr e env chan;
                env
  | Comp (op, g, d) ->
                begin
                compileExpr g env chan;
                compileExpr d env chan;
                match op with
                  Eq ->   output_string chan "EQUAL\n"
                | Neq ->  output_string chan "EQUAL\nNOT\n"
                | Lt ->   output_string chan "INF\n"
                | Le ->   output_string chan "INFEQ\n"
                | Gt ->   output_string chan "SUP\n"
                | Ge ->   output_string chan "SUPEQ\n"
                ;env
                end
  | Cast (s, el) -> output_string chan "\t\t-- cast\n";env
  | Selec (e, s) -> output_string chan "\t\t-- selec\n";env
  | Call (e, s, el) -> 
                List.iter (fun ex -> compileExpr ex env chan) el;
                let nomCO = "..." in (*TODO : trouver nom classe objet de e *)
                let eti = find_eti_methode nomCO s el in
                output_string chan ("CALL "^eti^"\n")
  | Inst (s, el) -> output_string chan "\t\t-- inst\n";env
                

(* SUB INSTR *)


and compileReturn env chan  =
  output_string chan "\t\t-- compileReturn\n";
  


(* exp : evaluation expression, th : then instr, el : else instr *)
and compileIte exp th el env chan  =
  let (etiElse, etiFin) = makeEtiITE () in
  compileExpr exp env chan;
  output_string chan "JZ "; output_string chan etiElse;
  output_string chan "\n";
  compileInstr th env chan;
  output_string chan "JUMP "; output_string chan etiFin;
  output_string chan "\n";
  output_string chan etiElse; output_string chan ": NOP\n";
  compileInstr el env chan;
  output_string chan etiFin; output_string chan ": NOP\n";

(* exp1, exp2 *)
and compileAssign exp1 exp2 env chan  =
  output_string chan "\t\t-- compileAssign\n";
  


and compileInstr i env chan  =
(* output_string chan "\t-- compileInstr\n"; *)
  match i with
    Expr exp -> compileExpr exp env chan
  | Bloc bl -> compileBloc bl env chan
  | Return exp -> (match exp with
        None -> compileReturn env chan
      | Some exp -> compileReturn (compileExpr exp env chan) chan)
  | Ite (exp, th, el) -> compileIte exp th el env chan
  | Assign (exp1, exp2) -> compileAssign exp1 exp2 env chan


(* SUB BLOC *)


(* d : current declaration, env : environment, chan : buffer *)
(*TODO  : pour l'instant modifie pas env*)
and compileDecl d env chan  =
  output_string chan "\t-- compileDecl\n";
  


and compileBloc bl env chan =
  let (ld, li) = bl in
(* output_string chan "\tcompileBloc\n"; *)
  let rec compileLDecl ld env chan =
    match ld with
      [] -> env
    | he::ta -> compileLDecl ta (compileDecl he env chan) chan
  and compileLInstr li env chan =
    match li with
      [] -> env
    | he::ta -> compileLInstr ta (compileInstr he env chan) chan
  in
  compileLInstr li (compileLDecl ld env chan) chan
;;


let compile codl main chan =

  let rec compileLCO codl env =
    let compileClassOrObj co env chan =
      match co with
        Class c -> compileClass c env chan
      | Objet o -> compileObject o env chan
    in
    match codl with
      [] -> env
    | he::ta -> compileLCO ta (compileClassOrObj he env chan)
  and compileMain main env chan =
    compileBloc main env chan
  in
  output_string chan "START\n";
  compileMain main (compileLCO codl ()) chan;
  output_string chan "STOP\n";
  flush chan;
  close_out chan;
;;
