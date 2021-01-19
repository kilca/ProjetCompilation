open Ast

(* --------------- Types et fonctions utiles ------------------------- *)


type methParam = string*string list;;

let table = Eval.table;;

let cptEtiITE = ref 0;; (* compteur: Instruction globale *)
let cptEtiMeth = ref 0;; (* compteur : Declaration globale *)
let cptIdCO = ref 0;; (* compteur : id Classe et Object globale *)

let makeEtiMethod nomMethode = (* generateur d'etiquettes fraiches pour methodes *)
  cptEtiMeth := !cptEtiMeth + 1;
  "METHOD" ^ (string_of_int !cptEtiMeth) ^ ": NOP --" ^ nomMethode
;;

let makeEtiITE () = (* generateur d'etiquettes fraiches pour ITE *)
  let sv = string_of_int !cptEtiITE in
  cptEtiITE := !cptEtiITE + 1;
  ("ELSE : NOP" ^ sv, "ENDIF : NOP" ^ sv)  (* ^ est l'operateur de concatenation de strings *)
;;

let rec findMethodType (nomCO :string) (nomMethode : string) (args : Ast.expType list) =
  let argsString = List.map (fun x -> expr_to_string x) args in
  if (Hashtbl.mem !table.objet nomCO) then (*si c'est un objet*)
    begin
      let objetH = Hashtbl.find !table.objet nomCO in
      let meth = Hashtbl.find objetH.meth (nomMethode,argsString) in
      match meth.typ with
        |None -> ""
        |Some x -> x
    end
  else (*sinon on considere que c'est une classe *)
    begin
      let classeH = Hashtbl.find !table.classe nomCO in
      let meth = Hashtbl.find classeH.meth (nomMethode,argsString) in
      match meth.typ with
        |None -> ""
        |Some x -> x
    end

and findAttributType (nomCO :string) (nomAttribut : string) =
  if (Hashtbl.mem !table.objet nomCO) then (*si c'est un objet*)
    begin
      let objetH = Hashtbl.find !table.objet nomCO in
      let attr = Hashtbl.find objetH.attr nomAttribut in
      attr.typ
    end
  else (*sinon on considere que c'est une classe *)
    begin
      let classeH = Hashtbl.find !table.classe nomCO in
      let attr = Hashtbl.find classeH.attr nomAttribut in
      attr.typ
    end

(* TODO : donner le type de l'expression 
  devra probablement prendre plus tard un autre arg env
*)
and expr_to_string exp =
  match exp with
    Id  i -> i (*TODO*)
  | ClassID i -> i (*TODO*)
  | Cste _ -> "Integer"
  | CsteStr _-> "String"
  | Plus _-> "Integer"
  | Minus _-> "Integer"
  | Times _-> "Integer"
  | Div _-> "Integer"
  | Concat _-> "String"
  | UMinus _-> "Integer"
  | UPlus _-> "Integer"
  | Comp _-> "Integer"
  | Cast (s,e) -> s (*TODO ?*)
  | Selec (e,s) -> findAttributType (expr_to_string e) s (*TODO chose en plus?*) 
  | Call (e,s,el)-> findMethodType (expr_to_string e) s el(*TODO chose en plus?*) 
  | Inst (s,el)-> "" (*TODO*)

(*[!] Todo : prendre en compte l'heritage *)
(* trouve l'etiquette a partir de la methode *)
let find_eti_methode nom nomMethode parametres =
  let paramTypes = List.map (fun x -> expr_to_string x) parametres in
  if (Hashtbl.mem !table.classe nom) then
    begin
      let (cCode : Eval.classHash) = Hashtbl.find !table.classe nom in
      Hashtbl.find cCode.methEti (nomMethode,paramTypes)
    end
  else
    begin
      let (oCode : Eval.objetHash) = Hashtbl.find !table.objet nom in
      Hashtbl.find oCode.methEti (nomMethode,paramTypes)
    end
;;

(*rempli les classCode et objectCode de !table.class et !table.object *)
(*
let fillObject o =
  Hashtbl.add !table.objet o.nom {
              data = o;
              attrIndex = Hashtbl.create 50;
              methEti = Hashtbl.create 50;
            }
and fillClass (c : Ast.classDecl) =
  Hashtbl.add !table.classe c.nom {
              data = c;
              attrIndex = Hashtbl.create 50;
              methEti = Hashtbl.create 50;
            }
;;
*)
(* ----------------------  Corp Generation Code ------------------------------ *)


let rec compileFunDecl f env chan =
  output_string chan "\t\t-- compileFunDecl\n";

  env


and compileConsDecl c env chan =
  output_string chan "\t\t-- compileConsDecl\n";
  env


and compileAttrib env chan =
  (*output_string chan "\t\t-- compileAttrib\n";*)
  
  (*TODO recuperer le nom de l'objet/classe et le nom de l'attribut afin de faire : *)
  (* --- Exemple ----*)
  let nomAttribut = "nomAttribut" in
  let nomObjet = "NomObjet" in

  let currObjet = (Hashtbl.find (!table.objet) nomObjet) in
  let currId = currObjet.attrCpt in
  let currIdStr = string_of_int (!currId) in
  Hashtbl.add currObjet.attrIndex nomAttribut !currId;

  output_string chan ("STORE "^currIdStr^"\n");
  currObjet.attrCpt := !(currObjet.attrCpt) + 1;
  env


and compileClassMember cm env chan =
  output_string chan "\t-- compileClassMember\n";
  match cm with
    Fun f -> compileFunDecl f env chan
  | Con c -> compileConsDecl c env chan
  | Att d -> compileAttrib (compileDecl d env chan) chan


and compileLClassMember lcm env chan =
  match lcm with
    [] -> env
  | he::ta -> compileLClassMember ta (compileClassMember he env chan) chan

(*j'ai mis dans une fonction a par au cas ou mais peut etre pareil que classe *)
and compileObjectMember cm env chan =
match cm with
  Fun f -> compileFunDecl f env chan
| Con c -> compileConsDecl c env chan
| Att d -> compileAttrib (compileDecl d env chan) chan


and compileLObjectMember lcm env chan =
match lcm with
  [] -> env
| he::ta -> 
output_string chan "DUPN 1\n";(*on dupplique l'adresse de l'objet pour la store *)
compileLObjectMember ta (compileObjectMember he env chan) chan

(* obj : object, env : environment (structure abstraite), chan : string buffer *)
and compileObject obj env chan =
  
  output_string chan ("   -- compileObject "^obj.nom^"\n");
  
  (*a mettre dans compileClassMember? *)  
  let currentHash = Hashtbl.find !table.objet obj.nom in
  currentHash.index := !cptIdCO;
  cptIdCO := !cptIdCO + 1;
  let nombreAttribut = Hashtbl.length currentHash.attr in
  let nombreAttributStr = string_of_int nombreAttribut in

  output_string chan ("ALLOC "^nombreAttributStr^"-- On alloue nb declarations\n");
  compileLObjectMember (obj.cbl) env chan;
  env


(* cls : class, env : environment (structure abstraite), chan : string buffer *)
and compileClass cls env chan =
  output_string chan "-- compileClass\n";
  (*fillClass cls;*)
  (* on appelera makeEtiMeth *)
  compileLDecl cls.para env chan;
  compileLClassMember cls.cbl env chan

(*PS : pour l'instant seul le cas Object est geree*)
(* exp : expType *)
and compileExpr exp env chan  =
(* output_string chan "\t\t-- compileExpr\n"; *)
  match exp with
    Id s -> output_string chan "\t\t\t-- Id\n";
            env
  | ClassID s -> 
                let indexObjet = (Hashtbl.find !table.objet s).index in
                output_string chan ("PUSHG "^(string_of_int !indexObjet)^" -- On empile l'adresse de "^s^"\n");
                 env
  | Cste i -> output_string chan "PUSHI ";
              output_string chan (string_of_int i);
              output_string chan "\n";
              env
  | CsteStr s -> output_string chan "PUSHS \"";
                 output_string chan s;
                 output_string chan "\"\n";
                 env
  | Plus (e1, e2) -> compileExpr e1 env chan;
                     compileExpr e2 env chan;
                     output_string chan "ADD\n";
                     env
  | Minus (e1, e2) -> compileExpr e1 env chan;
                      compileExpr e2 env chan;
                      output_string chan "SUB\n";
                      env
  | Times (e1, e2) -> compileExpr e1 env chan;
                      compileExpr e2 env chan;
                      output_string chan "MUL\n";
                      env
  | Div (e1, e2) -> compileExpr e1 env chan;
                    compileExpr e2 env chan;
                    output_string chan "DIV\n";
                    env
  | Concat (e1, e2) -> compileExpr e1 env chan;
                       compileExpr e2 env chan;
                       output_string chan "CONCAT\n";
                       env
  | UMinus e -> output_string chan "PUSHI 0\n";
                compileExpr e env chan;
                output_string chan "SUB\n";
                env
  | UPlus e -> compileExpr e env chan;
               env
  | Comp (op, g, d) -> begin
                        compileExpr g env chan;
                        compileExpr d env chan;
                        match op with
                          Eq ->   output_string chan "EQUAL\n"
                        | Neq ->  output_string chan "EQUAL\nNOT\n"
                        | Lt ->   output_string chan "INF\n"
                        | Le ->   output_string chan "INFEQ\n"
                        | Gt ->   output_string chan "SUP\n"
                        | Ge ->   output_string chan "SUPEQ\n"
                        ; env
                      end
  | Cast (s, el) -> output_string chan "\t\t\t-- cast\n";
                    env
  | Selec (e, s) -> (*Todo : seul le cas object est geree *)
                    let nomObjet = expr_to_string e in
                    let objetHash = Hashtbl.find !table.objet nomObjet in
                    let idAttr = Hashtbl.find objetHash.attrIndex s in
                    compileExpr e env chan (*Todo? utiliser retour ?*);
                    output_string chan ("LOAD "^(string_of_int idAttr)^" -- On charge l'attribut "^s^"\n");
                    
                    env
  | Call (e, s, el) ->
                      begin
                      let expType = expr_to_string e in
                      match expType with
                      | "String" ->
                                  
                                    if (s = "print") then 
                                    begin
                                      compileExpr e env chan;
                                      output_string chan ("WRITES \n");
                                      env
                                    end
                                    else if (s = "println") then
                                    begin
                                      compileExpr e env chan;
                                      output_string chan ("WRITES \n");
                                      output_string chan ("PUSHS \" \\n \" \n");
                                      output_string chan ("WRITES \n"); 
                                      env 
                                    end
                      | "Integer" ->
                                    if (s = "toString") then
                                    compileExpr e env chan;
                                    output_string chan ("STR \n");
                                    env
                      | _->  (*Todo *)
                      
                                (*List.iter (fun ex -> compileExpr ex env chan) el;
                                  let nomCO = "..." in (*TODO : trouver nom classe objet de e *)
                                  let eti = find_eti_methode nomCO s el in
                                  output_string chan ("CALL " ^ eti ^ "\n");*)
                                  env
                      end
  | Inst (s, el) -> output_string chan "\t\t\t-- inst\n";
                    env


(* SUB INSTR *)


and compileReturn env chan  =
  output_string chan "\t\t-- compileReturn\n";

  env


(* exp : evaluation expression, th : then instr, el : else instr *)
and compileIte exp th el env chan  =
  let (etiElse, etiFin) = makeEtiITE () in
  compileExpr exp env chan;
  output_string chan "JZ ";
  output_string chan etiElse;
  output_string chan "\n";
  compileInstr th env chan;
  output_string chan "JUMP ";
  output_string chan etiFin;
  output_string chan "\n";
  output_string chan etiElse;
  output_string chan ": NOP\n";
  compileInstr el env chan;
  output_string chan etiFin;
  output_string chan ": NOP\n";
  env


(* exp1, exp2 *)
and compileAssign exp1 exp2 env chan  =
  output_string chan "\t\t-- compileAssign\n";

  env


and compileInstr i env chan  =
(* output_string chan "\t\t-- compileInstr\n"; *)
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
  (*output_string chan "\t\t-- compileDecl\n";*)
  match d.rhs with
  |Some e -> compileExpr e env chan
  | None -> (*???*)
  env


and compileLDecl ld env chan =
    match ld with
      [] -> env
    | he::ta -> compileLDecl ta (compileDecl he env chan) chan


and compileBloc bl env chan =
  let (ld, li) = bl in
(* output_string chan "compileBloc\n"; *)
  let rec compileLInstr li env chan =
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
