open Ast

(* obj : object, env : environment (structure abstraite), chan : string buffer *)
let rec compileObject obj env chan =
  output_string chan "compileObject\n";

(* cls : class, env : environment (structure abstraite), chan : string buffer *)
and compileClass cls env chan =
  output_string chan "compileClass\n";

(* d : current declaration, env : environment, chan : buffer *)
and compileDecl d env chan  =
  output_string chan "\tcompileDecl\n";

(* exp : expType *)
and compileExpr exp env chan  =
  output_string chan "\t\tcompileExpr\n";


(* bl : blocType *)
and compileBloc bl env chan  =
  output_string chan "\t\tcompileBloc\n";


and compileReturn env chan  =
  output_string chan "\t\tcompileReturn\n";


(* exp : evaluation expression, th : then instr, el : else instr *)
and compileIte exp th el env chan  =
  output_string chan "\t\tcompileIte\n";


(* exp1, exp2 *)
and compileAssign exp1 exp2 env chan  =
  output_string chan "\t\tcompileAssign\n";

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
  and compileMain main env =
    let (ld, li) = main in
    let rec compileLDecl ld env =
      match ld with
        [] -> env
      | he::ta -> compileLDecl ta (compileDecl he env chan)
    and compileLInstr li env =
      let rec compileInstr i env chan  =
        match i with
          Expr exp -> compileExpr exp env chan
        | Bloc bl -> compileBloc bl env chan
        | Return exp -> compileReturn (compileExpr exp env chan) chan
        | Ite (exp, th, el) -> compileIte exp th el env chan
        | Assign (exp1, exp2) -> compileAssign exp1 exp2 env chan
      in
      match li with
        [] -> env
      | he::ta -> compileLInstr ta (compileInstr he env chan)
    in
    compileLInstr li (compileLDecl ld env)
  in
  output_string chan "START\n";
  compileMain main (compileLCO codl ());
  output_string chan "STOP\n";
  flush chan;
  close_out chan;
;;
