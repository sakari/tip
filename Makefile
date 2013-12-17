tests = $(patsubst %.tip,%.test,$(wildcard examples/*.tip))
failing = $(patsubst %.tip,%.fail,$(wildcard examples-errors/*.tip))

%.test : %.tip
	dist/build/tip/tip $< | node

%.fail : %.tip
	dist/build/tip/tip $< > /dev/null ; test "$$?" -ne 0

all : build
	$(MAKE) test

test: $(tests) $(failing)

build:
	cabal build

.PHONY: test build
