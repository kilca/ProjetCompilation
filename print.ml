open Ast

   (* imprime une expression sous forme entierement parenthésée, de façon à
    * ce qu'on puisse facilement verifier si les précédences et associativités
    * demandées sont bien respectées.
    *)

(*let rec printExpr e =
  match e with
      Id s -> print_string s
   | ClassID s -> print_string s
    | Cste i -> print_int i
    | Plus(g, d) ->
       print_string "["; printExpr g; print_string " + ";
       printExpr d; print_string "]"
    | Minus (g, d) ->
       print_string "["; printExpr g; print_string " - ";
       printExpr d; print_string "]"
    | Times (g, d) ->
       print_string "["; printExpr g; print_string " * ";
       printExpr d; print_string "]"
    | Div (g, d) ->
       print_string "["; printExpr g; print_string " / ";
       printExpr d; print_string "]"
    | UMinus e -> print_string "[ - ";  printExpr e; print_string "]"
    | Comp(op, g, d) ->
       print_string "["; printExpr g;
       print_string (Misc.string_of_relop op); printExpr d; print_string "]"
    | Cast (s,e) ->
      print_string "["; printExpr e; print_string " As ";
      print_string s; print_string "]"
   | Call (s,l,e) -> 
         print_string "[";print_string s; print_string (".");
         print_string l;print_string "(";
         List.iter (fun d -> printExpr d;print_string ",") e;
         print_string ")]"
   |None -> print_string "";
    | _ -> print_string ""




let printInit x=
match x with
|VarInit a-> printExpr a;
|ClassInit (a,b)-> print_string " NEW (";List.iter (fun d -> printExpr d;print_string ",") b;print_string ")"

let printDecl d = 

   
  print_string d.lhs;
  print_string ":"; 
  print_string d.typ;
  match d.rhs with
  | Some x -> print_string " :="; printInit x;
   | None -> ();
  

   print_newline ();
;;*)