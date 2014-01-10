.PHONY: all
all: dist/setup-config Hotel.hs
	runhaskell -W Hotel.hs

dist/setup-config: Language/* Language/GIGL/*
	cabal install

.PHONY: clean
clean:
	cabal clean
	-rm hotel.lisp

