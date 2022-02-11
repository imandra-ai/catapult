#!/bin/sh
exec dune exec --profile=release src/sqlite/bin/catapult_conv.exe -- $@
