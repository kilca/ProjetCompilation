open Ast

(* obj : object, env : environment (structure abstraite), chan : string buffer *)
let rec compileObject obj env chan =
  output_string chan "-- compileObject\n";
  


(* cls : class, env : environment (structure abstraite), chan : string buffer *)
and compileClass cls env chan =
  output_string chan "-- compileClass\n";
  


(* exp : expType *)
and compileExpr exp env chan  =
(* output_string chan "\t\t-- compileExpr\n"; *)
  match exp with
    Id s -> output_string chan "\t\t-- Id\n";
  | ClassID s -> output_string chan "\t\t-- ClassID\n";
  | Cste i -> output_string chan "\t\t-- constInt\n";
  | CsteStr s -> output_string chan "\t\t-- constString\n";
  | Plus (e1, e2) -> compileExpr e1 env chan; 
                    compileExpr e2 env chan; 
                    output_string chan "ADD\n";
  | Minus (e1, e2) -> output_string chan "\t\t-- Minus\n";
  | Times (e1, e2) -> output_string chan "\t\t-- Times\n";
  | Div (e1, e2) -> output_string chan "\t\t-- Div\n";
  | Concat (e1, e2) -> output_string chan "\t\t-- Concat\n";
  | UMinus e -> output_string chan "\t\t-- UMinus\n";
  | UPlus e -> output_string chan "\t\t-- UPlus\n";
  | Comp (op, e1, e2) -> output_string chan "\t\t-- Comp\n";
  | Cast (s, e) -> output_string chan "\t\t-- cast\n";
  | Selec (e, s) -> output_string chan "\t\t-- selec\n";
  | Call (e, s, el) -> output_string chan "\t\t-- call\n";
  | Inst (s, el) -> output_string chan "\t\t-- inst\n";


(* SUB INSTR *)


and compileReturn env chan  =
  output_string chan "\t\t-- compileReturn\n";
  


(* exp : evaluation expression, th : then instr, el : else instr *)
and compileIte exp th el env chan  =
  output_string chan "\t\t-- compileIte\n";
  


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
