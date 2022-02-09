#!/bin/sh
export TRACE=1
exec dune exec --profile=release examples/heavy/heavy.exe -- $@
