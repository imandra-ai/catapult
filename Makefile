
OPTS=--profile=release
all:
	@dune build @all $(OPTS)

test:
	@dune runtest --force $(OPTS)

clean:
	@dune clean

fmt:
	@dune build @fmt --display=quiet --auto-promote

WATCH ?= @all
watch:
	@dune build $(WATCH) -w $(OPTS)
