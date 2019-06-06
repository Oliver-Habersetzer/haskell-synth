build:
	cabal v2-build fp-project

clean:
	cabal v2-clean
	rm -rf build
	rm -rf dist

run:
	cabal v2-run fp-project

test:
	cabal v2-test test

repl:
	cabal v2-repl fp-project

test-repl:
	cabal v2-repl test

dist:
	cabal v2-sdist