open Ast

(*
---Verificateur Contextuel---

Quasiment rien pour l'instant
*)

(*Pour declarer variable "globale" et eviter de le redonner a chaque parametre*)
type tableCO = 
  { mutable classe : ((string, Ast.classDecl) Hashtbl.t); mutable objet : ((string, objetDecl) Hashtbl.t) };;
let table = ref { classe = Hashtbl.create 50; objet = Hashtbl.create 50 };;

let eval ld e = ()
  ;;
