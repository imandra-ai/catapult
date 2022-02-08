#!/bin/sh
exec dune exec --profile=release examples/file/basic.exe -- $@
