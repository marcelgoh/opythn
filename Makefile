BUILD = ocamlbuild

FLAGS = -r ${USEFLAGS} ${PKGFLAGS}
USEFLAGS = -use-ocamlfind -use-menhir
PKGFLAGS = -pkg extlib

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
