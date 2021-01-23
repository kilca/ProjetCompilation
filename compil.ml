open Ast

(* --------------- Types et fonctions utiles ------------------------- *)

type methParam = string*string list;;

type choixBloc = Classe | Objet | Main;;

(*ce que je proposerais pour env *)
(*probleme : chiant a assigner et modifier les valeurs *)
(*
type envType = {
  location : choixBloc;
  variablesLoc:((string, int) Hashtbl.t);
  maxIdDecl : int;
  nomMethode : string;
  nomCO : string;
};;
*)

(* id, type *)
type envT = ((string, (int*string)) Hashtbl.t);;

let table = Eval.table;;

let cptEtiITE = ref 0;; (* compteur: Instruction globale *)
let cptEtiMeth = ref 0;; (* compteur : Declaration methode globale *)
let cptIdCO = ref 0;; (* compteur : id Classe et Object globale *)
let cptIdDecl = ref 1;; (*compteur : id Declaration locale globale*)


let makeEtiMethod nomMethode = (* generateur d'etiquettes fraiches pour methodes *)
  cptEtiMeth := !cptEtiMeth + 1;
  "METHOD" ^ (string_of_int !cptEtiMeth)
;;

let makeEtiITE () = (* generateur d'etiquettes fraiches pour ITE *)
  let sv = string_of_int !cptEtiITE in
  cptEtiITE := !cptEtiITE + 1;
  ("ELSE"^sv, "ENDIF"^sv)  (* ^ est l'operateur de concatenation de strings *)
;;

let rec findMethodType (nomCO :string) (nomMethode : string) (args : Ast.expType list)  (env : envT)=
  let argsString = List.map (fun x -> expr_to_string x env) args in
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

and findAttributType (nomCO :string) (nomAttribut : string)  (env : envT)=
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
and expr_to_string exp  (env : envT) =
  match exp with
    Id  i -> 
        if (Hashtbl.mem env i) then
        begin
          let (id,typ) = Hashtbl.find env i in
          typ
        end
        else
        begin
          print_string ("Todo : cas this et ??? : "^i);
          i
        end
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
  | Selec (e,s) -> findAttributType (expr_to_string e env) s env(*TODO chose en plus?*) 
  | Call (e,s,el)-> findMethodType (expr_to_string e env) s el env(*TODO chose en plus?*) 
  | Inst (s,el)-> "" (*TODO*)

and storeDecl expr env chan=
match expr with
Id  i -> 
    if (Hashtbl.mem env i) then
    begin
      let (id,typ) = Hashtbl.find env i in
      output_string chan ("STOREL "^(string_of_int id)^" -- store variable "^i^"\n");
    end
    else
    begin
      print_string ("Todo : cas this et ??? : "^i);
    end
| ClassID i -> ()(*erreur ?*)
| Cast (s,e) -> () (*TODO ?*)
| Selec (e,s) -> ()
| Call (e,s,el)-> () (*TODO *) 
|_ -> failwith "WTF LEFT ASSIGN";
;;
(*[!] Todo : prendre en compte l'heritage *)
(* trouve l'etiquette a partir de la methode *)
(*retourne (etiquette,Ast.fundecl) *)
let find_eti_methode nom nomMethode parametres  (env : envT) =
  let paramTypes = List.map (fun x -> expr_to_string x env) parametres in
  if (Hashtbl.mem !table.classe nom) then (*dans classe *)
    begin
      let (cCode : Eval.classHash) = Hashtbl.find !table.classe nom in
      (Hashtbl.find cCode.methEti (nomMethode,paramTypes), Hashtbl.find cCode.meth (nomMethode,paramTypes))
    end
  else (*dans objet *)
    begin
      let (oCode : Eval.objetHash) = Hashtbl.find !table.objet nom in
      (Hashtbl.find oCode.methEti (nomMethode,paramTypes), Hashtbl.find oCode.meth (nomMethode,paramTypes))
    end
;;

(* ----------------------  Corp Generation Code ------------------------------ *)


let rec compileFunDecl (objectName:string) (f : Ast.funDecl) (env : envT) chan =
  output_string chan "\t\t-- compileFunDecl\n";
  let eti = makeEtiMethod f.nom in
  
  output_string chan (eti^": NOP--- "^(f.nom)^"\n");
  
  let hashO = Hashtbl.find !table.objet objectName in
  let mparam = (f.nom,List.map (fun (x:Ast.decl) ->x.typ) f.para) in
  Hashtbl.add hashO.methEti mparam eti;

  let nbArgs = List.length f.para in
  let idRetour =  ((-nbArgs)-1) in
  let variables = Hashtbl.create 50 in

  (*si la fonction retourne qqc on declare result*)
  match f.typ with
  |None->();
  |Some x-> Hashtbl.add variables "result" (idRetour,x);
  ;

  (*equivalent d'un for *)
  let rec aux (para : Ast.decl list) i =
    match para with
    | [] -> ()
    | x::s -> begin(*on push les arguments de la fonction *)
               (*output_string chan ("PUSHL "^(string_of_int i)^" --recup arg\n");*)
              (*output_string chan ("STOREL "^(string_of_int i)^" --stocker valeurArg\n");*)(*load 0 dans exemple ??*)
              
              (*les arguments sont deja compris en tant que definission du bloc *)
              Hashtbl.add variables x.lhs (i,x.typ);
              aux s (i+1);
              end
  in aux (f.para) (-nbArgs);

  cptIdDecl:= 0;
  let _ = compileBloc (f.corp) variables chan in
  let _ = compileReturn variables chan in
  match f.typ with
  | None -> (); (*Todo : ??? *)
  | Some x ->
            (*output_string chan ("STOREL "^(string_of_int ((-nbArgs)-1))^" -- resultat\n"); *)  
            (*
            output_string chan ("PUSHL "^(string_of_int (-nbArgs))^" -- resultat\n");
            output_string chan ("SWAP -- swap adresse et val\n");
            output_string chan ("STORE 0 -- stocker resultat\n");
            *)
            ();
;
  env;


and compileConsDecl c (env : envT) chan =
  output_string chan "\t\t-- compileConsDecl\n";
  env


and compileAttrib objectName decl (env : envT) chan =
 
  let _=
  match decl.rhs with
  |Some e -> compileExpr e env chan
  | None -> env (*???*)
  in

  let nomAttribut = decl.lhs in
  let nomObjet = objectName in

  let currObjet = (Hashtbl.find (!table.objet) nomObjet) in
  let currId = currObjet.attrCpt in
  let currIdStr = string_of_int (!currId) in
  Hashtbl.add currObjet.attrIndex nomAttribut !currId;

  output_string chan ("STORE "^currIdStr^"\n");
  currObjet.attrCpt := !(currObjet.attrCpt) + 1;
  env


and compileClassMember cm (env : envT) chan =
  output_string chan "\t-- compileClassMember\n";
  match cm with
    Fun f -> compileFunDecl "" f env chan
  | Con c -> compileConsDecl c env chan
  | Att d -> compileAttrib "" d (compileDecl d env chan) chan


and compileLClassMember lcm (env : envT) chan =
  match lcm with
    [] -> env
  | he::ta -> compileLClassMember ta (compileClassMember he env chan) chan

(*j'ai mis dans une fonction a par au cas ou mais peut etre pareil que classe *)
and compileObjectMember objectName cm (env : envT) chan =
  match cm with
    Fun f -> compileFunDecl objectName f env chan
  | Con c -> compileConsDecl c env chan
  | Att d -> compileAttrib objectName d env chan

(*les parametres en plus seraient a mettre dans env *)
and compileLObjectMember objectName lcm (env : envT) chan =
  match lcm with
    [] -> env
  | he::ta -> 
  output_string chan "DUPN 1\n";(*on dupplique l'adresse de l'objet pour la store *)
  compileLObjectMember objectName ta (compileObjectMember objectName he env chan) chan

(* obj : object, env : environment (structure abstraite), chan : string buffer *)
and compileObject obj (env : envT) chan =
  
  output_string chan ("   -- compileObject "^obj.nom^"\n");
  
  (*a mettre dans compileClassMember? *)  
  let currentHash = Hashtbl.find !table.objet obj.nom in
  currentHash.index := !cptIdCO;
  cptIdCO := !cptIdCO + 1;
  let nombreAttribut = Hashtbl.length currentHash.attr in
  let nombreAttributStr = string_of_int nombreAttribut in

  output_string chan ("ALLOC "^nombreAttributStr^"-- On alloue nb declarations\n");
  let _ = compileLObjectMember obj.nom (obj.cbl) env chan in
  env

(* cls : class, env : environment (structure abstraite), chan : string buffer *)
and compileClass cls (env : envT) chan =
  output_string chan "-- compileClass\n";
  (*fillClass cls;*)
  (* on appelera makeEtiMeth *)
  let nEnv = compileLDecl cls.para env chan in
  compileLClassMember cls.cbl nEnv chan

(*PS : pour l'instant seul le cas Object est geree*)
(* exp : expType *)
and compileExpr exp (env : envT) chan  =
(* output_string chan "\t\t-- compileExpr\n"; *)
  match exp with
    Id s -> 
            if (Hashtbl.mem env s) then (*cas variable locale *)
            begin
              let (id,typ) = Hashtbl.find env s in
              output_string chan ("PUSHL "^(string_of_int id)^" -- On get la variable de "^s^"\n");(*LOAD*)
              (*
              output_string chan ("DUPN 1 -- On duplique la variable de "^s^"\n");(*LOAD*)
              output_string chan ("STOREL "^(string_of_int id)^" -- On restock la variable de "^s^"\n");(*LOAD*)
              *)
              env
            end
            else
            begin
              print_string "cas this et autre";
              env
            end
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
  | Plus (e1, e2) -> let _ = compileExpr e1 env chan in
                     let _ = compileExpr e2 env chan in
                     output_string chan "ADD\n";
                     env
  | Minus (e1, e2) -> let _ = compileExpr e1 env chan in
                      let _ = compileExpr e2 env chan in
                      output_string chan "SUB\n";
                      env
  | Times (e1, e2) -> let _ = compileExpr e1 env chan in
                      let _ = compileExpr e2 env chan in
                      output_string chan "MUL\n";
                      env
  | Div (e1, e2) -> let _ = compileExpr e1 env chan in
                    let _ = compileExpr e2 env chan in
                    output_string chan "DIV\n";
                    env
  | Concat (e1, e2) -> let _ = compileExpr e1 env chan in
                       let _ = compileExpr e2 env chan in
                       output_string chan "CONCAT\n";(*Todo : Autre chose*)
                       env
  | UMinus e -> output_string chan "PUSHI 0\n";
                let _ = compileExpr e env chan in
                output_string chan "SUB\n";
                env
  | UPlus e -> compileExpr e env chan;
  | Comp (op, g, d) -> begin
                        let _ = compileExpr g env chan in
                        let _ = compileExpr d env chan in
                        match op with
                          Eq ->   output_string chan "EQUAL\n";env
                        | Neq ->  output_string chan "EQUAL\nNOT\n";env
                        | Lt ->   output_string chan "INF\n";env
                        | Le ->   output_string chan "INFEQ\n";env
                        | Gt ->   output_string chan "SUP\n";env
                        | Ge ->   output_string chan "SUPEQ\n";env
                      end
  | Cast (s, el) -> output_string chan "\t\t\t-- cast\n";
                    env
  | Selec (e, s) -> (*Todo : seul le cas object est geree *)
                    let typeExpr = expr_to_string e env in
                    let objetHash = Hashtbl.find !table.objet typeExpr in
                    let idAttr = Hashtbl.find objetHash.attrIndex s in
                    let _ = compileExpr e env chan in(*Todo? utiliser retour ?*)
                    output_string chan ("LOAD "^(string_of_int idAttr)^" -- On charge l'attribut "^s^"\n");
                    
                    env
  | Call (e, s, el) ->
                      let expType = expr_to_string e env in
                      begin
                      match expType with
                      | "String" ->
                                  
                                    if (s = "print") then 
                                    begin
                                      let _ = compileExpr e env chan in
                                      output_string chan ("WRITES \n");
                                      env
                                    end
                                    else if (s = "println") then
                                    begin
                                      let _ = compileExpr e env chan in
                                      output_string chan ("WRITES \n");
                                      output_string chan ("PUSHS \"\\n\"\n");
                                      output_string chan ("WRITES \n"); 
                                      (*
                                      output_string chan ("PUSHS \"\\n\"\n");
                                      output_string chan ("CONCAT \n");
                                      output_string chan ("WRITES \n"); 
                                      *)
                                      env 
                                    end
                                    else env
                      | "Integer" ->
                                    if (s = "toString") then
                                    begin
                                    let _ = compileExpr e env chan in
                                    output_string chan ("STR \n");
                                    env
                                    end
                                    else env
                      | _->  (*Todo *)
                                begin
                                let nomCO = expr_to_string e env in
                                let nbArgs = List.length el in
                                let (eti,meth) = find_eti_methode nomCO s el env in
                                if (meth.typ <> None) 
                                (*then output_string chan ("ALLOC 1 --On prepare le retour de la fonction\n");*)
                                then output_string chan ("PUSHN 1 --On prepare le retour de la fonction\n")
                                else output_string chan ("--la fonction retourne rien\n")
                                ;
                                List.iter (fun ex -> let _ = compileExpr ex env chan in ()) el;
                                output_string chan ( "PUSHA "^eti^"--addr fonction \n"); 
                                output_string chan ("CALL --appel fonction \n");
                                output_string chan ("POPN " ^ string_of_int nbArgs ^ "--Depiler les arguments\n");
                                (*TODO : LOAD ?*)
                                env
                                end
                    end
  | Inst (s, el) -> output_string chan "\t\t\t-- inst\n";
                    env


(* SUB INSTR *)


and compileReturn (env : envT) chan  =
  output_string chan "\t\t-- compileReturn\n";
  if (not (Hashtbl.mem env "result")) then failwith "return without result"
  else begin
    let (id,typ) = Hashtbl.find env "result" in
    output_string chan ("STOREL "^(string_of_int id)^" -- resultat fonction\n");  
    output_string chan ("RETURN --on repart\n")
  end 
  ;
  env


(* exp : evaluation expression, th : then instr, el : else instr *)
and compileIte exp th el (env : envT) chan  =
  let (etiElse, etiFin) = makeEtiITE () in
  let _ = compileExpr exp env chan in
  output_string chan "JZ "; output_string chan etiElse;
  output_string chan "\n";
  let _ = compileInstr th env chan in
  output_string chan "JUMP "; output_string chan etiFin;
  output_string chan "\n";
  output_string chan etiElse;
  output_string chan ": NOP\n";
  let _ = compileInstr el env chan in
  output_string chan etiFin;
  output_string chan ": NOP\n";
  env


(* exp1, exp2 *)
and compileAssign exp1 exp2 (env : envT) chan  =
  (*si c'est une variable globale alors STOREG, sinon STOREL *)
  let _ = compileExpr exp2 env chan in
  let _ = storeDecl exp1 env chan in
  env


and compileInstr i (env : envT) chan  =
(* output_string chan "\t\t-- compileInstr\n"; *)
  match i with
    Expr exp -> compileExpr exp env chan
  | Bloc bl -> compileBloc bl (Hashtbl.copy env) chan
  | Return exp -> 
                begin
                match exp with
                  None -> compileReturn env chan
                | Some exp -> compileReturn (compileExpr exp env chan) chan
                end
  | Ite (exp, th, el) -> compileIte exp th el env chan
  | Assign (exp1, exp2) -> compileAssign exp1 exp2 env chan


(* SUB BLOC *)

(* d : current declaration, env : environment, chan : buffer *)
(*TODO  : pour l'instant modifie pas env*)
(*s'occupe de faire les declLocales *)
and compileDecl d (env : envT) chan  =
  (*output_string chan "\t\t-- compileDecl\n";*)
  match d.rhs with
  |Some e -> let _ = compileExpr e env chan in
              output_string chan ("STOREL "^(string_of_int (!cptIdDecl))^" --On Stock la variable \n" );
              Hashtbl.add env d.lhs ((!cptIdDecl),d.typ);
              cptIdDecl := (!cptIdDecl) + 1;
              env
  | None ->   
              match d.typ with
              | "Integer" -> 
              output_string chan ("PUSHI 0 --On Init la variable \n" );
              | "String" ->
              output_string chan ("PUSHS \"\" --On Init la variable \n" );
              | _ -> ();
              ;
              output_string chan ("STOREL "^(string_of_int (!cptIdDecl))^" --On Stock la variable \n" );
              Hashtbl.add env d.lhs ((!cptIdDecl),d.typ);
              cptIdDecl := (!cptIdDecl) + 1;
              env


and compileLDecl ld (env : envT) chan =
    match ld with
      [] -> env
    | he::ta -> compileLDecl ta (compileDecl he env chan) chan


(*env : variables locales*)
and compileBloc bl (env : envT) chan =
  let (ld, li) = bl in
  let rec compileLInstr li (en : envT) chan =
    match li with
      [] -> env
    | he::ta -> compileLInstr ta (compileInstr he en chan) chan
  in
  let tempEnv = compileLDecl ld (Hashtbl.copy env) chan in
  compileLInstr li tempEnv chan
;;


let compile codl main chan =
  let rec compileLCO codl (env : envT) =
    let compileClassOrObj co (env : envT) chan =
      match co with
        Class c -> compileClass c env chan
      | Objet o -> compileObject o env chan
    in
    match codl with
      [] -> env
    | he::ta -> compileLCO ta (compileClassOrObj he env chan)
  and compileMain main (env : envT) chan =
    output_string chan "Main: NOP      --Debut Main\n";
    compileBloc main env chan
  in
  output_string chan "START\n";
  output_string chan "JUMP Main\n";  
  cptIdDecl := 1;
  let _ = compileMain main (compileLCO codl (Hashtbl.create 50)) chan in
  output_string chan "STOP\n";
  flush chan;
  close_out chan;
;;
