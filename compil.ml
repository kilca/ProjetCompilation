open Ast

let rec compileExpr e env chan  = () (* a faire *)

;;


let compile ld e =
  let chan = open_out "out" in
  output_string chan "START\n";
  output_string chan "STOP\n";
  flush chan;
  close_out chan;
;;
