.PHONY: all
all: dist/setup-config Hotel.hs
	runhaskell -W Hotel.hs

dist/setup-config: Language/*
	cabal install

.PHONY: clean
clean:
	cabal clean

