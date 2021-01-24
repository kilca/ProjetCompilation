open Ast
open Eval
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
let currCO = ref "";; (* compteur d'instances *)

let makeEtiMethod nomMethode = (* generateur d'etiquettes fraiches pour methodes *)
  cptEtiMeth := !cptEtiMeth + 1;
  "METHOD" ^ (string_of_int !cptEtiMeth)

and makeEtiClassOrObj () =
  let retour = "ClObj_" ^ (string_of_int !cptIdCO) ^ ": NOP\n" in
  cptIdCO := !cptIdCO + 1;
  retour

and makeEtiITE () = (* generateur d'etiquettes fraiches pour ITE *)
  let sv = string_of_int !cptEtiITE in
  cptEtiITE := !cptEtiITE + 1;
  ("ELSE"^sv, "ENDIF"^sv)  (* ^ est l'operateur de concatenation de strings *)
;;

let rec findMethodType (nomCO :string) (nomMethode : string) (args : Ast.expType list)  (env : envT)=
  let argsString = List.map (fun x -> expr_to_string x env nomCO) args in
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
and expr_to_string exp  (env : envT) (currentClass : string)=
  match exp with
    Id  i ->
        if (i = "this") then (!currCO)
        else
          begin
            if (Hashtbl.mem env i) then
            begin
              let (id,typ) = Hashtbl.find env i in
              typ
            end
            else
            begin
              findAttributType currentClass i env
            end
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
  | Selec (e,s) -> findAttributType (expr_to_string e env s) s env(*TODO chose en plus?*) 
  | Call (e,s,el)-> findMethodType (expr_to_string e env s) s el env(*TODO chose en plus?*) 
  | Inst (s,el)-> s

and storeDecl expr env chan=
	match expr with
		Id  i ->
      if (Hashtbl.mem env i) then
        begin
          let (id,typ) = Hashtbl.find env i in
          output_string chan ("\tSTOREL "^(string_of_int id)^" -- store variable "^i^"\n");
        end
      else
        begin
          (*Todo : revoir *)
          if (i = "this") then
            begin
              print_string "Todo : trouver id ";
            end
          else
            begin
              print_string "Todo : ???";
            end
        end
  | ClassID i -> (*on push l'adresse de l'objet*)
          let hashO = (Hashtbl.find !table.objet i) in
          let ind = !(hashO.index) in
          output_string chan ("\tPUSHG "^(string_of_int (ind))^" -- push objet"^i^"\n");
  | Cast (s,e) -> () (*TODO ?*)
  | Selec (e,s) ->  (*Todo : pour l'instant marche que avec objet *)
          let _ = storeDecl e env chan in
          output_string chan ("\tSWAP \n");
          let nomObjet = expr_to_string e env "" in
          let hashO = (Hashtbl.find !table.objet nomObjet) in
          let ind = Hashtbl.find hashO.attrIndex s in
          output_string chan ("\tSTORE "^(string_of_int (ind))^" -- stocke variable "^s^"\n")
  | Call (e,s,el)-> () (*TODO *)
  |_ -> failwith "WTF LEFT ASSIGN";
;;
(*[!] Todo : prendre en compte l'heritage *)
(* trouve l'etiquette a partir de la methode *)
(*retourne (etiquette,Ast.fundecl) *)
let find_eti_methode nom nomMethode parametres  (env : envT) =
  let paramTypes = List.map (fun x -> expr_to_string x env nom) parametres in
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
  let idRetour =  ((-nbArgs)-2) in
  let variables = Hashtbl.create 50 in

  Hashtbl.add variables "this" (-1,objectName);

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
               (*output_string chan ("\tPUSHL "^(string_of_int i)^" --recup arg\n");*)
              (*output_string chan ("\tSTOREL "^(string_of_int i)^" --stocker valeurArg\n");*)(*load 0 dans exemple ??*)

              (*les arguments sont deja compris en tant que definission du bloc *)
              Hashtbl.add variables x.lhs (i,x.typ);
              aux s (i+1);
              end
  in aux (f.para) (-nbArgs-1);

  cptIdDecl:= 0;
  let _ = compileBloc (f.corp) variables chan in
  let _ = compileReturn variables chan in
  match f.typ with
  | None -> (); (*Todo : ??? *)
  | Some x ->
            (*output_string chan ("\tSTOREL "^(string_of_int ((-nbArgs)-1))^" -- resultat\n"); *)
            (*
            output_string chan ("\tPUSHL "^(string_of_int (-nbArgs))^" -- resultat\n");
            output_string chan ("\tSWAP -- swap adresse et val\n");
            output_string chan ("\tSTORE 0 -- stocker resultat\n");
            *)
            ();
;
  env;


and compileConsDecl (c : consDecl) (env : envT) chan =
  (*Todo *)
  (*Todo : Creer etiquette *)
  (*Todo call constructeur parent *)
  (*Todo declarer les index des attributs de la classe exemple :*)
  
  (*
  let hashC = Hashtbl.find !table.classe c.nom in

  match c.superrr with
  | Some x -> let hashCParent = Hashtbl.find !table.classe x.ex in
              hashC.attrIndex = Hashtbl.copy (hashCParent.attrIndex);
              hashC.attrCpt := !hashCParent.attrCpt;
  | None -> ();
  ;
  
  Hashtbl.iter (fun nom decl -> 
    if (not (Hashtbl.mem hashC nom)) then
    begin
      Hashtbl.add (hashC.attr) nom !hashC.attrCpt; 
      hashC.attrCpt := !hashC.attrCpt + 1;
    end
  
  ) hashC.attr
    
    *)
    (*Todo : assigner les parametres du constructeur aux attributs : (les outputs seront les suivants)*)
    
    (*
  let currId = ref 0 in
    Hashtbl.iter (fun nom decl -> 
    let _ =
      match decl.rhs with
      |Some e -> let _ = 
                output_string chan ("\tDUPN 1\n");
                compileExpr e env chan in
                let currIdStr = string_of_int (!currId) in
                output_string chan ("\tSTORE "^currIdStr^"\n");
                env
      | None -> env
      in
      currId := (!currId + 1);

  ) hashC.attr;
  env
  *)

  (*Todo : compiler corp fonction *)
  env

and compileAttrib objectName decl (env : envT) chan =
  let nomObjet = objectName in  
  let currObjet = (Hashtbl.find (!table.objet) nomObjet) in  
  let nomAttribut = decl.lhs in
  let currId = currObjet.attrCpt in
  match decl.rhs with
  |Some e -> (*si declare*)
          output_string chan "\tDUPN 1 -- DUPN compileAttrib\n";(*on dupplique l'adresse de l'objet pour la store *)
          
          let retour = compileExpr e env chan in
        
          let currIdStr = string_of_int (!currId) in
          Hashtbl.add currObjet.attrIndex nomAttribut !currId;
        
          output_string chan ("\tSTORE "^currIdStr^"\n");
          currObjet.attrCpt := !(currObjet.attrCpt) + 1;
          retour
  | None ->  (*si non declare *)
          Hashtbl.add currObjet.attrIndex nomAttribut !currId;
          currObjet.attrCpt := !(currObjet.attrCpt) + 1;
          env (*Todo : ???*)


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
  | Con c -> failwith "error object with constructor"
  | Att d -> compileAttrib objectName d env chan


(*les parametres en plus seraient a mettre dans env *)
and compileLObjectMember objectName lcm (env : envT) chan =
  match lcm with
    [] -> env
  | he::ta ->
  (*output_string chan "\tDUPN 1 -- DUPN compileLObjectMember\n"; *)(* on duplique l'adresse de l'objet pour la store *)
  compileLObjectMember objectName ta (compileObjectMember objectName he env chan) chan


(* obj : object, env : environment (structure abstraite), chan : string buffer *)
and compileObject obj (env : envT) chan =
  output_string chan ("\t-- compileObject " ^ obj.nom ^ "\n");

  (*a mettre dans compileClassMember? *)
  let currentHash = Hashtbl.find !table.objet obj.nom in
  currentHash.index := !cptIdCO;

  output_string chan (makeEtiClassOrObj ());

  let nombreAttribut = Hashtbl.length currentHash.attr in
  let nombreAttributStr = string_of_int nombreAttribut in

  output_string chan ("\tALLOC "^nombreAttributStr^"-- On alloue nb declarations\n");
  let _ = compileLObjectMember obj.nom (obj.cbl) env chan in
  env


(* cls : class, env : environment (structure abstraite), chan : string buffer *)
and compileClass cls (env : envT) chan =
  output_string chan "-- compileClass\n";
  (*fillClass cls;*)
  output_string chan ("JUMP EndCl_" ^ (string_of_int (!cptIdCO + 1)) ^ "\n"); (* has not been incremented yet *)
  output_string chan (makeEtiClassOrObj ());
  (* TODO ALLOC *)
  let nEnv = compileLDecl cls.para env chan in
  let _ = compileLClassMember cls.cbl nEnv chan in
  output_string chan ("EndCl_" ^ (string_of_int !cptIdCO) ^ ": NOP\n");
  env;


(*PS : pour l'instant seul le cas Object est geree*)
(* exp : expType *)
and compileExpr exp (env : envT) chan  =
(* output_string chan "\t\t-- compileExpr\n"; *)
  match exp with
    Id s ->
            if (Hashtbl.mem env s) then (*cas variable locale *)
            begin
              let (id,typ) = Hashtbl.find env s in
              output_string chan ("\tPUSHL "^(string_of_int id)^" -- On get la variable de "^s^"\n");(*LOAD*)
              (*
              output_string chan ("\tDUPN 1 -- On duplique la variable de "^s^"\n");(*LOAD*)
              output_string chan ("\tSTOREL "^(string_of_int id)^" -- On restock la variable de "^s^"\n");(*LOAD*)
              *)
              env
            end
            else
            begin
              print_string "cas this et autre"; (* TODO *)
              env
            end
  | ClassID s ->
                let indexObjet = (Hashtbl.find !table.objet s).index in
                output_string chan ("\tPUSHG "^(string_of_int !indexObjet)^" -- On empile l'adresse de "^s^"\n");
                 env
  | Cste i -> output_string chan "\tPUSHI ";
              output_string chan (string_of_int i);
              output_string chan "\n";
              env
  | CsteStr s -> output_string chan "\tPUSHS \"";
                 output_string chan s;
                 output_string chan "\"\n";
                 env
  | Plus (e1, e2) -> let _ = compileExpr e1 env chan in
                     let _ = compileExpr e2 env chan in
                     output_string chan "\tADD\n";
                     env
  | Minus (e1, e2) -> let _ = compileExpr e1 env chan in
                      let _ = compileExpr e2 env chan in
                      output_string chan "\tSUB\n";
                      env
  | Times (e1, e2) -> let _ = compileExpr e1 env chan in
                      let _ = compileExpr e2 env chan in
                      output_string chan "\tMUL\n";
                      env
  | Div (e1, e2) -> let _ = compileExpr e1 env chan in
                    let _ = compileExpr e2 env chan in
                    output_string chan "\tDIV\n";
                    env
  | Concat (e1, e2) -> let _ = compileExpr e1 env chan in
                       let _ = compileExpr e2 env chan in
                       output_string chan "\tCONCAT\n";(*Todo : Autre chose*)
                       env
  | UMinus e -> output_string chan "\tPUSHI 0\n";
                let _ = compileExpr e env chan in
                output_string chan "\tSUB\n";
                env
  | UPlus e -> compileExpr e env chan;
  | Comp (op, g, d) -> begin
                        let _ = compileExpr g env chan in
                        let _ = compileExpr d env chan in
                        match op with
                          Eq ->   output_string chan "\tEQUAL\n";env
                        | Neq ->  output_string chan "\tEQUAL\nNOT\n";env
                        | Lt ->   output_string chan "\tINF\n";env
                        | Le ->   output_string chan "\tINFEQ\n";env
                        | Gt ->   output_string chan "\tSUP\n";env
                        | Ge ->   output_string chan "\tSUPEQ\n";env
                      end
  | Cast (s, el) -> output_string chan "\t\t\t-- cast\n";
                    env
  | Selec (e, s) -> (*Todo : seul le cas object est geree *)
                    let typeExpr = expr_to_string e env s in
                    let objetHash = Hashtbl.find !table.objet typeExpr in
                    let idAttr = Hashtbl.find objetHash.attrIndex s in
                    let _ = compileExpr e env chan in(*Todo? utiliser retour ?*)
                    output_string chan ("\tLOAD "^(string_of_int idAttr)^" -- On charge l'attribut "^s^"\n");

                    env
  | Call (e, s, el) ->
                      let expType = expr_to_string e env s in
                      begin
                      match expType with
                      | "String" ->
                                    let _ = compileExpr e env chan in (*On prepare le this *)
                                    if (s = "print") then
                                    begin
                                      output_string chan ("\tWRITES \n");
                                      env
                                    end
                                    else if (s = "println") then
                                    begin
                                      output_string chan ("\tWRITES \n");
                                      output_string chan ("\tPUSHS \"\\n\"\n");
                                      output_string chan ("\tWRITES \n");
                                      (*
                                      output_string chan ("\tPUSHS \"\\n\"\n");
                                      output_string chan ("\tCONCAT \n");
                                      output_string chan ("\tWRITES \n");
                                      *)
                                      env
                                    end
                                    else env
                      | "Integer" ->
                                    let _ = compileExpr e env chan in (*On prepare le this *)
                                    if (s = "toString") then
                                    begin
                                    output_string chan ("\tSTR \n");
                                    env
                                    end
                                    else env
                      | _->  (*Todo *)
                                begin
                                let nomCO = expr_to_string e env s in
                                let nbArgs = List.length el in
                                let (eti,meth) = find_eti_methode nomCO s el env in
                                if (meth.typ <> None)
                                (*then output_string chan ("\tALLOC 1 --On prepare le retour de la fonction\n");*)
                                then output_string chan ("\tPUSHN 1 --On prepare le retour de la fonction\n")
                                else output_string chan ("--la fonction retourne rien\n")
                                ;
                                List.iter (fun ex -> let _ = compileExpr ex env chan in ()) el;
                                let _ = compileExpr e env chan in (*On prepare le this *)
                                output_string chan ("\tPUSHA "^eti^"--addr fonction \n");
                                output_string chan ("\tCALL --appel fonction \n");
                                output_string chan ("\tPOPN " ^ string_of_int (nbArgs+1) ^ "--Depiler les arguments\n");
                                (*TODO : LOAD ?*)
                                env
                                end
                    end
  | Inst (s, el) -> 	 
                      let hashC = Hashtbl.find !table.classe s in
                      let nb = Hashtbl.length hashC.attr in
                      output_string chan ("\tALLOC "^(string_of_int nb)^"\n");
                      output_string chan ("-- instantier un objet de classe"^s^"\n");
                      env
                      (*Todo : serait un call du constructeur (et rien d'autre ??*)
                      
  (*TODO *)

(* SUB INSTR *)


and compileReturn (env : envT) chan  =
  output_string chan "\t\t-- compileReturn\n";
  if (Hashtbl.mem env "result") then
  begin
    let (id,typ) = Hashtbl.find env "result" in
    output_string chan ("\tSTOREL "^(string_of_int id)^" -- resultat fonction\n");
    output_string chan ("RETURN --on repart\n")
  end
  else
    output_string chan ("RETURN --on repart\n")
  ;
  env


(* exp : evaluation expression, th : then instr, el : else instr *)
and compileIte exp th el (env : envT) chan  =
  let (etiElse, etiFin) = makeEtiITE () in
  let _ = compileExpr exp env chan in
  output_string chan "\tJZ "; output_string chan etiElse;
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
              output_string chan ("\tSTOREL "^(string_of_int (!cptIdDecl))^" --On Stock la variable \n" );
              Hashtbl.add env d.lhs ((!cptIdDecl),d.typ);
              cptIdDecl := (!cptIdDecl) + 1;
              env
  | None ->
              match d.typ with
              | "Integer" ->
              output_string chan ("\tPUSHI 0 --On Init la variable \n" );
              | "String" ->
              output_string chan ("\tPUSHS \"\" --On Init la variable \n" );
              | _ -> ();
              ;
              output_string chan ("\tSTOREL "^(string_of_int (!cptIdDecl))^" --On Stock la variable \n" );
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
  print_int !cptIdDecl;
  let tempEnv = compileLDecl ld (Hashtbl.copy env) chan in
  compileLInstr li tempEnv chan
;;


let compile codl main chan =
  let rec compileLCO codl (env : envT) =
    let compileClassOrObj co (env : envT) chan =
      match co with
        Class c -> (currCO := c.nom);
                    compileClass c env chan
      | Objet o -> (currCO := o.nom);
                    compileObject o env chan
    in
    match codl with
      [] -> env
    | he::ta -> compileLCO ta (compileClassOrObj he env chan)
  and compileMain main (env : envT) chan =
    currCO := "";
    cptIdDecl:= 0;
    output_string chan "Main: NOP      --Debut Main\n";
    compileBloc main env chan
  in
  output_string chan "START\n";
  cptIdDecl := 1;
  let _ = compileMain main (compileLCO codl (Hashtbl.create 50)) chan in
  output_string chan "STOP\n";
  flush chan;
  close_out chan;
;;
