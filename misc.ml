open Ast

let string_of_relop (op: Ast.opComp)  =
  match op with
    Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
