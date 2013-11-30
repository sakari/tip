tests = $(patsubst %.tip,%.test,$(wildcard examples/*.tip))

%.test : %.tip
	dist/build/tip/tip $<

all : build
	$(MAKE) test

test: $(tests)

build:
	cabal build

.PHONY: test build
