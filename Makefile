
OPTS=--profile=release --ignore-promoted-rules
all:
	@dune build @all $(OPTS)

test:
	@dune runtest --force $(OPTS)

clean:
	@dune clean

fmt:
	@dune build @fmt --display=quiet --auto-promote

lint:
	@dune build @lint

WATCH ?= @all
watch:
	@dune build $(WATCH) -w $(OPTS)
