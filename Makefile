
all:
	runhaskell Setup configure --user
	runhaskell Setup build

install: all
	runhaskell Setup install

haddock: all
	cabal haddock

sdist:
	cabal sdist

clean:
	rm -rf dist
