STACK=stack --allow-different-user
GHCI_COMMAND=$(STACK) exec -- ghci

.PHONY: test build ghci ghcid docs

all: test

test:
	$(STACK) test

build:
	$(STACK) build

ghcid:
	ghcid --command="$(GHCI_COMMAND)"

ghci:
	$(GHCI_COMMAND)

docs:
	$(STACK) haddock --open
