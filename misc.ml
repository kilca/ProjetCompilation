open Ast

let string_of_relop (op: Ast.opComp)  =
  match op with
    Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

(*plus utile*)
(*
let is_first_uppercase (s : string) =
  if (s = "") then false
  else begin
    let c : char = s.[0] in
    (Char.code(c) >= Char.code('A') && Char.code(c) <= Char.code('Z'))
  end
;;
*)