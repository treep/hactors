
all:
	runhaskell Setup configure --user
	runhaskell Setup build

install:
	runhaskell Setup install

sdist:
	cabal sdist

clean:
	rm -rf dist
