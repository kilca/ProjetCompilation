{
open Ast
open TpParse
open Lexing
exception Eof

(* gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf

(* cree une table de hachage qu'on remplit avec le token associe
 * a chaque mot-clef
 *)
let keyword_table = Hashtbl.create 16
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "if", IF;
      "then", THEN;
      "else", ELSE;
      "return", RETURN;
      "class", CLASS;
      "extends", EXTENDS;
      "is", IS;
      "var", VAR;
      "def", DEF;
      "object", OBJECT;
      "override", OVERRIDE;
      "as", AS;
      "new", NEW;
    ]
}

let lettre = ['A'-'Z' 'a'-'z']
let lettreMin = ['a'-'z']
let lettreMaj = ['A'-'Z']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )

rule
 comment = parse
             "*/" { (* fin de commentaire trouvee. Le commentaire ne doit pas
                     * etre transmis. On renvoie donc ce que nous renverra un
                     * nouvel appel a l'analyseur lexical
                     *)
                    token lexbuf
                  }
  | '\n'           { (* incremente le compteur de ligne et poursuit la
                      * reconnaissance du commentaire en cours
                      *)
                     new_line lexbuf; comment lexbuf
                   }
  | eof            { (* detecte les commentaires non fermes pour pouvoir
                      * faire un message d'erreur clair.
                      * On pourrait stocker la position du dernier commentaire
                      * encore ouvert pour ameliorer le dioagnostic
                      *)
                     failwith "unclosed comment";
                   }
  | _              { (* rien a faire de special pour ce caractere, donc on
                      * poursuit la reconnaissance du commentaire en cours
                      *)
                     comment lexbuf
                   }

and
 quote buf = parse
  | '"'            { CSTESTRING (Buffer.contents buf) }
  | '\n'   { 
                    failwith "end of line not allowed"
                   }
  | '\\' 'n'         { 
                     Buffer.add_char buf '\n'; quote buf lexbuf
                   }
  | '\\' 't'         { 
                     Buffer.add_char buf '\t'; quote buf lexbuf
                   }
  | '\\' 'r'         { 
                     Buffer.add_char buf '\r'; quote buf lexbuf
                   }
  | '\\' 'f'         { 
                     Buffer.add_char buf '\012'; quote buf lexbuf
                   }
  | '\\' 'b'         { 
                     Buffer.add_char buf '\b'; quote buf lexbuf
                   }
  | '\\' '\\'         { 
                     Buffer.add_char buf '\\'; quote buf lexbuf
                   }
  | '\\' '/'         { 
                     Buffer.add_char buf '/'; quote buf lexbuf
                   }
  | '\r'  { failwith "end of line not allowed" }

  | '\\' '"'  { 
                     Buffer.add_char buf '\"'; quote buf lexbuf
                   }
  
  | eof            { 
                     failwith "unclosed quotation mark";
                   }
  | [^ '"' '\\' '\n' '\r']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      quote buf lexbuf
    }
  | _ { failwith "Illegal string character: " }

  and
 token = parse
      lettreMin LC * as id
      { (* id contient le texte reconnu. On verifie s'il s'agit d'un mot-clef
         * auquel cas on renvoie le token associe. Sinon on renvoie Id avec le
         * texte reconnu en valeur 
         *)
        try
          Hashtbl.find keyword_table id
        with Not_found -> ID id
      }
  | lettreMaj LC * as id
      { (* pas besoin de verifier car les mot clef
          car les mot clef ne commencent Jamais par une majuscule  
         *)
        CLASSID id
      }
  | [' ''\t''\r']+  { (* consommer les delimiteurs, ne pas les transmettre
                       * et renvoyer ce que renverra un nouvel appel a
                       *  l'analyseur lexical
                       *)
                       token lexbuf
                    }
  | '\n'           { next_line lexbuf; token lexbuf}
  | chiffre+ as lxm { CSTEINT (int_of_string lxm) }
  | "/*"           { comment lexbuf }
  | '"'            { quote (Buffer.create 17) lexbuf} 
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '&'            { CONCAT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'						 { LACCO }
  | '}'						 { RACCO }
  | ';'            { SEMICOLON }
  | ':'            { COLON }
  | ','            { COMMA }
  | "."            { DOT }
  | ":="           { ASSIGN }
  | "<"		         { RELOP (Ast.Lt) }
  | "<="           { RELOP (Ast.Le) }
  | ">"            { RELOP (Ast.Gt) }
  | ">="           { RELOP (Ast.Ge) }
  | "="            { RELOP (Ast.Eq) }
  | "<>"           { RELOP (Ast.Neq) }
  | eof            { EOF }
  | _ as lxm       { (* action par défaut: filtre un unique caractere, different
                      * de ceux qui precedent. Il s'agit d'un caratere errone:
                      * on le signale et on poursuit quand meme l'analyse
                      *)
                     print_endline
                       ("undefined character: " ^ (String.make 1 lxm));
                     token lexbuf
                   }
