open Ast

(* co : object or class, env : environment (structure abstraite), chan : string buffer *)
let rec compileClassOrObj co env chan =
  output_string chan "compileClassOrObj\n"

(* d : current declaration, env : environment, chan : buffer *)
and compileDecl d env chan  =
  output_string chan "compileDecl\n"

(* i : current instr*)
and compileInstr i env chan  =
  output_string chan "compileInstr\n"
;;

let compile codl main chan =
  let rec compileLCO codl env =
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
