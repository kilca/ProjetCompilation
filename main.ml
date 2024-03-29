open Ast
open Lexing


let parse_with_error lexbuf file_in chan =
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" file_in
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  try
    let codl, main = TpParse.prog TpLex.token lexbuf in (* classObjectDeclarationList, main programm function *)
    
    Eval.eval codl main;
    Print.print_progType (codl, main);
    Compil.compile codl main chan;
    
  with
    TpParse.Error ->
    Printf.fprintf stderr "Syntax error at position %a\n" print_position lexbuf;
    exit (-1)

let _ =
  let argc = Array.length Sys.argv in
  if argc = 1 then
    print_endline "usage: tp programme [fichier-pour-le-code] "
  else
    begin
      (* si on ne passe pas à l'appel le nom du fichier dans lequel
       * ecrire le code produit, on utilise par défaut le fichier "out.txt"
       *)
      let file_out = if argc = 3 then Sys.argv.(2) else "out.cfvs" (* cfvs ; Compiled Frederic Voisin's Script *)
      and file_in = Sys.argv.(1) in
      let chan_in = open_in file_in
      and chan_out = open_out file_out in
      let lexbuf = Lexing.from_channel chan_in in
      parse_with_error lexbuf file_in chan_out;
      close_in chan_in; close_out chan_out
    end
