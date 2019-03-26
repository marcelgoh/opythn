BUILD = ocamlbuild -r -use-ocamlfind

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
	${BUILD} ${PKGFLAGS} $@
