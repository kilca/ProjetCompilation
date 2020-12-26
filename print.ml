open Ast

   (* imprime une expression sous forme entierement parenthésée, de façon à
    * ce qu'on puisse facilement verifier si les précédences et associativités
    * demandées sont bien respectées.
    *)

let print_opComp (p : Ast.opComp) =
   match p with
    Eq -> print_string "=="
   | Neq -> print_string "<>"
   | Lt -> print_string "<"
   | Le -> print_string "<="
   | Gt -> print_string ">"
   | Ge -> print_string ">="
;;  
  
let print_const (p : Ast.const)=
 print_string "CONST[";
 match p with
 Int i -> print_int i;print_string "]"
 | String s -> print_string s;print_string "]"
;;
  

let rec print_expType (e : Ast.expType) =
   match e with
       Id s -> print_string "ID[";print_string s;print_string "]"
    | ClassID s -> print_string s
     | Cste i -> print_const i
     | Plus(g, d) ->
        print_string "["; print_expType g; print_string " + ";
        print_expType d; print_string "]"
     | Minus (g, d) ->
        print_string "["; print_expType g; print_string " - ";
        print_expType d; print_string "]"
     | Times (g, d) ->
        print_string "["; print_expType g; print_string " * ";
        print_expType d; print_string "]"
     | Div (g, d) ->
        print_string "["; print_expType g; print_string " / ";
        print_expType d; print_string "]"
        | Concat (g, d) ->
        print_string "["; print_expType g; print_string " & ";
        print_expType d; print_string "]"
     | UMinus e -> print_string "[ - ";  print_expType e; print_string "]"
     | UPlus e -> print_string "[ + ";  print_expType e; print_string "]"
     | Comp(op, g, d) ->
        print_string "["; print_expType g;
        print_string (Misc.string_of_relop op); print_expType d; print_string "]"
     | Cast (s,e) ->
       print_string "["; print_expType e; print_string " As ";
       print_string s; print_string "]"
    | Call (s,l,e) -> 
          print_string "[";print_expType s; print_string (".");
          print_string l;print_string "(";
          List.iter (fun d -> print_expType d;print_string ",") e;
          print_string ")]"
   | Inst (s,l) -> print_string "[NEW ";print_string s;
   print_string "("; List.iter (fun d -> print_expType d;print_string ",") l; print_string ")]"
   | Selec(e,i) -> print_expType e; print_string "."; print_string i;
;;


let print_decl (d : Ast.decl) = 

   if (d.isVar) then print_string " VAR ";
   print_string d.lhs;
   print_string ":"; 
   print_string d.typ;
   match d.rhs with
    Some x -> print_string " :="; print_expType x;
   | None -> ();
   
;;

let print_paramDecl (p : Ast.paramDecl) =
   List.iter (fun d -> print_string "  ";print_decl d;print_string ",") p
;;
  
let rec print_blocType (p : Ast.blocType)=
   print_string "Bloc{";
   print_newline ();
   List.iter (fun d -> print_string "  ";print_decl d;print_newline ()) (fst p);
   print_string "IS";
   print_newline ();
   List.iter (fun d -> print_string "  ";print_instr d;print_newline ()) (snd p);
   print_string "}"
and
print_instr i= 
   match i with
   Expr e -> print_expType e
   | Bloc bl -> print_blocType bl;
   | Return e -> begin
      match e with
      Some x -> print_string "RETURN [";print_expType x;print_string "]"
   | None -> print_string "DefaultReturn";
   end
   | Ite (e,r,t) -> print_string "ITE("; print_expType e; print_string ",";
   print_instr r;print_string ",";print_instr t;print_string ")"
   | Assign (e,r)-> print_expType e;print_string "=";print_expType r
;;

let print_superO (s : Ast.superO)=
   print_string "SUPER";
   print_string s.ex;
   print_string "(";
   List.iter (fun d -> print_expType d;print_string ",") s.para;
   print_string ")"
;;
  
let print_consDecl (p : Ast.consDecl)=
   print_string "[Constructor ";
   print_string p.nom;
   print_string " (";
   print_paramDecl p.para;
   print_string " )";
   print_newline ();
   match p.superrr with
      Some x ->   print_blocType p.bloc;
      print_string "]";
    | None ->    print_blocType p.bloc;
    print_string "]";
;;

let print_funDecl (p : Ast.funDecl)=
   print_string "FUN[";
   print_string p.nom;
   if (p.over) then print_string " OVER";
   print_string " (";
   print_paramDecl p.para;
   print_string " )";
   print_string " :";
   match p.typ with
      Some x -> print_string x;   print_instr p.corp;
      print_string "]"
   | None ->  print_instr p.corp;
   print_string "]"
;;

let print_confun (p : Ast.membreClasse) =
   match p with
   Fun x -> print_funDecl x
   |Con x -> print_consDecl x
   |Att x -> print_decl x
   
;;
  
let print_classBloc (p : membreClasse list) =
   print_string "[";
   List.iter (fun d -> print_string "  ";print_confun d;print_newline ()) p;
   print_newline ();
   print_string "]"
;;
  
let print_classDecl (p : classDecl) =
   print_string "CLASS[ ";

   print_string p.nom;
   print_string " (";
   print_paramDecl (p.para);
   print_string " )";
   match p.ext with
    Some x -> print_string "EXTENDS ";print_string x;
    print_newline ();
    print_classBloc p.cbl;
    print_string "]"
   | None -> print_newline ();
   print_classBloc p.cbl;
   print_string "]";
;;
  
let print_objetDecl p =
   print_string "OBJET[ ";
   print_string p.nom;
   print_newline ();
   List.iter (fun d -> print_confun d;print_newline ()) p.cbl;
   print_string "]"
;;

let print_classObjDecl p =
   match p with
   Class x -> print_classDecl x
   |Objet x -> print_objetDecl x
;;

let print_progType p =
   List.iter (fun d -> print_classObjDecl d;print_newline ()) (fst p);
   print_blocType (snd p);
;;

