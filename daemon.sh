#!/bin/sh
exec dune exec --profile=release src/daemon/catapult_daemon.exe -- $@
