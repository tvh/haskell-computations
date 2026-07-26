.PHONY: all test

# -Werror is off by default in package.yaml (a warning on a newer GHC must
# never break a downstream consumer's build), but this repo's own build/CI
# path should still catch new warnings. Override with `make STACK_FLAGS=`
# to build without it.
STACK_FLAGS ?= --flag incremental-computations:werror

all:
	stack build $(STACK_FLAGS)

test:
	stack test $(STACK_FLAGS)
