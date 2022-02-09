#!/bin/sh
export TRACE=1
exec dune exec --profile=release examples/file/basic.exe -- $@
