PWSH = pwsh

.PHONY: build
build: build-deps
	cabal v2-build $(CABAL_OPTIONS) optparse-declarative

.PHONY: build-deps
build-deps:
	cabal v2-build --only-dependencies $(CABAL_OPTIONS)

.PHONY: test
test: test-example-simple test-example-subcmd test-example-verbose test-example-nonstrargs

.PHONY: test-example-simple
test-example-simple: build
	cabal v2-build $(CABAL_OPTIONS) optparse-declarative:exe:example-simple
	cabal exec -- example-simple Tanaka

.PHONY: test-example-subcmd
test-example-subcmd: build
	cabal v2-build $(CABAL_OPTIONS) optparse-declarative:exe:example-subcmd
	cabal exec -- example-subcmd greet Tanaka

.PHONY: test-example-verbose
test-example-verbose: build
	cabal v2-build $(CABAL_OPTIONS) optparse-declarative:exe:example-verbose
	cabal exec -- example-verbose -v3

.PHONY: test-example-nonstrargs
test-example-nonstrargs: build
	cabal v2-build $(CABAL_OPTIONS) optparse-declarative:exe:example-nonstrargs
	cabal exec -- example-nonstrargs 1 2 3

.PHONY: repl
repl:
	cabal v2-repl $(CABAL_OPTIONS)

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, app, test | ForEach-Object { stylish-haskell -i $$_.FullName } }"
	stylish-haskell -i Setup.hs

.PHONY: setup-format
setup-format:
	cabal v2-install stylish-haskell --overwrite-policy=always

.PHONY: lint
lint:
	hlint src
	hlint app

.PHONY: setup-lint
setup-lint:
	cabal v2-install hlint --overwrite-policy=always

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: clean
clean:
	cabal v2-clean
