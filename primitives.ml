open Ast


let print =
  {
    nom = "print";
    para = [];
    typ =None;
    over = false;
    corp = ([],[]);
  };;
 let println =
   {
     nom = "println";
     para = [];
     typ =None;
     over = false;
     corp = ([],[]);
   };;
let toString =
  {
    nom = "toString";
    para = [];
    typ = Some("String");
    over = false;
    corp = ([],[]);
  };;

let stringCons =
  {
    nom = "String";
    para= [];
    superrr = None;
    bloc= ([],[]);
  }
let integerCons =
  {
    nom = "Integer";
    para= [];
    superrr = None;
    bloc= ([],[]);
  }

let integerc =
  {
    nom = "Integer";
    para = [];
    ext = None;
    cbl =[Fun toString; Con integerCons];
  };;

let stringc =
  {
    nom = "String";
    para = [];
    ext = None;
    cbl =[Fun print; Fun println; Con stringCons];
  };;
