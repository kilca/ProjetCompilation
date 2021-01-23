# ProjetCompilation


# Problemes lies a Ocaml : 

comme env n'est plus unit il faut preciser au compilateur que parfois on s'en fout de la valeur de retour de compile.

Cela se foit avec let _ = ... in
soit avec ignore ()

c'est chiant et moche a geree ...

--------

Todo :

-ne pas push les expressions inutiles : genre 42; (pour l'instant laisser l'erreur et la traiter plus tard)