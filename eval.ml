open Ast


type tableCO = 
  { mutable classe : ((string, Ast.classDecl) Hashtbl.t); mutable objet : ((string, objetDecl) Hashtbl.t) };;
let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let eval ld e = 
  !table.classe
  ;;
