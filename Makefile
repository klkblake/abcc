build:
	cabal build
	idris --build abcc.ipkg

install:
	cabal install
	idris --install abcc.ipkg

clean:
	cabal clean
	idris --clean abcc.ipkg

.PHONY: build install clean
