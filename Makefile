
all:
	runhaskell Setup configure --user
	runhaskell Setup build

install:
	runhaskell Setup install

clean:
	rm -rf dist
