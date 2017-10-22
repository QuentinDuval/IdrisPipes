all: install

install: build
	idris --install pipes.ipkg

build: src/Pipes/*.idr
	idris --build pipes.ipkg

test: build
	(cd tests; bash tests.sh)

clean:
	idris --clean pipes.ipkg
	rm -f tests/*.ibc

tuto: build
	(cd tests; bash tutorial.sh)
