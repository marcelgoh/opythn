BUILD = ocamlbuild

FLAGS = -r ${USEFLAGS} ${PKGFLAGS}
USEFLAGS = -use-ocamlfind -use-menhir -menhir ${MENHIRFLAGS}
MENHIRFLAGS="menhir --explain --external-tokens Token"
PKGFLAGS = -pkg extlib -pkg ppx_deriving.show -pkg str -pkg unix

EXT = native
EXEC = main

${EXEC}: src/main.${EXT}
	cp _build/$< $@
	rm $@.${EXT}

.PHONY: clean
clean:
	${BUILD} -clean
	rm ${EXEC}

%:
	${BUILD} ${FLAGS} $@
