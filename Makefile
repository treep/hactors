
all:
	runhaskell Setup configure --user
	runhaskell Setup build

install: all
	runhaskell Setup install

doc: all
	cabal haddock
	mv ./dist/doc/html/hactors ./doc

tar:
	cabal sdist

clean:
	rm -rf dist
