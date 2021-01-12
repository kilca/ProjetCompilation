INTERFACES = tpParse.mli
SOURCES    = ast.ml primitives.ml tpParse.ml tpLex.ml misc.ml print.ml eval.ml compil.ml main.ml
GENERATED  = tpLex.ml tpParse.ml tpParse.mli tpParse.automaton tpParse.conflicts

tp: tpParse.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o tp $(SOURCES)

testLex : tpParse.mli tpLex.ml testLex.ml misc.ml
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o testLex ast.ml misc.ml print.ml eval.ml compil.ml tpParse.ml  tpLex.ml testLex.ml


tpLex.ml: tpLex.mll tpParse.mli ast.ml
	ocamllex tpLex.mll

tpParse.mli : tpParse.mly ast.ml
	menhir --dump --explain --strict tpParse.mly

clean:
	rm -rf  tp testLex *.o *.cmi *.cmo *.cmx *~ $(GENERATED)

cleanOut: clean
	rm -f out.txt
