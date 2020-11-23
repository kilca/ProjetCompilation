open Ast

let eval ld e =
  (* evalDecl: prend une liste de declarations et renvoie une liste
   * (variable, valeur) qui associe à chaque variable la valeur qui
   * resulte de l'evaluation de son expression d'initialisation.
   *
   * ld : la liste des declarations a traiter
   * env : la liste d'association des declarations deja traitees.
   *
   * On appelle initialement evalDecl avec env = [] et la liste de toutes
   * les declarations.
   *)
  let rec evalDecl ld env =
    match ld with
      [] -> env
    | (* { lhs; rhs; } est un raccourci pour { lhs = lhs; rhs = rhs }
       * C'est comme si on se servait des noms de champ comme variables locales
       * dans la decomposition du record.
       * Voir si besoin sur internet les records en ocaml.
       * On evalue la partie droite dans l'environnement courant avant d'ajouter
       * le nouveau couple (variable, valeur) à l'environnement pour l'appel
       * suivant. L'ordre des couples dans la liste d'importe pas tant qu'on
       * n'a pas deux declarations de la meme variable (ce qui devrait etre
       * verifie dans une phase de Verifications Contextuelles).
       *)
      { lhs; rhs; } :: ld' ->
       evalDecl ld' ((lhs, evalExpr rhs env) :: env)
  and evalExpr e env =
    match e with
      Id x ->
       (* L'exception ne pourrait pas arriver si on avait verifie qu'une
        * expression n'utilise que des variables deja declarees.
        *)
       begin
         try List.assoc x env
         with Not_found -> failwith ("variable non declaree: " ^ x)
       end
    | Cste v -> v
    | Plus(g, d) ->   (evalExpr g env) + (evalExpr d env)
    | Minus (g, d) -> (evalExpr g env) - (evalExpr d env)
    | Times (g, d) -> (evalExpr g env) * (evalExpr d env)
    | Div (g, d) ->   (evalExpr g env) / (evalExpr d env)
    | UMinus e ->     - (evalExpr e env)
    | Ite (si, alors, sinon) ->
       if (evalBexpr si env) then (evalExpr alors env) else (evalExpr sinon env)
    | (* Voir le commentaire ci-dessous *)
      _ -> failwith "Eval: structure AST incorrecte dans une expression"
  and evalBexpr e env =
    (* On aurait pu creer un type ocaml specifique pour les expressions de
     * comparaison. Ici on a utilise le meme type que pour les expressions
     * arithmetiques, mais une expression de comparaison, pour ce TP, ne peut
     * apparaitre que dans la condition d'un IF et syntaxiquement aura toujours
     * un operateur de comparaison a la racine de son AST. Donc si ici on a
     * autre chose qu'un Comp c'est qu'on a un probleme. Dans evalExpr c'etait
     * l'inverse: un operateur de comparaison ne pouvait apparaitre a la racine
     *)
    match e with
      Comp(op, g, d) ->
       begin
         let vg = evalExpr g env and vd = evalExpr d env in
         match op with
           Eq ->  vg = vd
         | Neq -> vg != vd
         | Lt ->  vg < vd
         | Le ->  vg <= vd
         | Gt ->  vg > vd
         | Ge ->  vg <= vd
       end
    | _ -> failwith "evalBexpr: structure AST incorrecte dans une comparaison"
  in evalExpr e (evalDecl ld [])
;;
