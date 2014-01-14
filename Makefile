.PHONY: all
all: all.lisp
	acl2 < all.lisp

all.lisp: hotel.lisp properties.lisp
	cat hotel.lisp properties.lisp > all.lisp

dist/setup-config: Language/* Language/GIGL/*
	cabal install

hotel.lisp: dist/setup-config Hotel.hs
	runhaskell -W Hotel.hs

.PHONY: clean
clean:
	cabal clean
	-rm hotel.lisp
	-rm all.lisp

