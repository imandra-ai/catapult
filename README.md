# Catapult [![build](https://github.com/imandra-ai/catapult/actions/workflows/main.yml/badge.svg)](https://github.com/imandra-ai/catapult/actions/workflows/main.yml)

This is a tracing library for OCaml, based on the
[Catapult/TEF](https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/)
trace format.


The traces are `.json` files (or compressed `.json.gz`). They can be viewed in:
- https://ui.perfetto.dev/
- chrome://tracing in chrome/chromium
- https://github.com/wolfpld/tracy after conversion (the `tracy-import-chrome` binary)

## Usage

The core library is `catapult`. It's a small set of probes that can be
inserted in your code, by hand (with meaningful messages if needed).

## Systemd

An example systemd service file can be found in `src/data/catapult-daemon.service`.

```systemd
[Unit]
Description=Catapult daemon (receives and stores profiling traces)

[Socket]
ListenStream=6981
Accept=no

[Service]
ExecStart=catapult-daemon --addr=tcp://127.0.0.1:6981
Restart=always
RestartSec=10

[Install]
WantedBy=default.target
```

### Example: "basic"

A very stupid example (in `examples/basic/basic.ml`), is:

```ocaml
module Tr = Catapult.Tracing
let spf = Printf.sprintf

let rec fake_trace depth =
  if depth>=3 then ()
  else (
    (* the probe is here *)
    Tr.with_ "step" @@ fun () ->
    Thread.delay 0.1;
    Printf.printf "fake (depth=%d)\n%!" depth;
    fake_trace (depth+1);
    Thread.delay 0.2;
    Tr.instant "iteration.done" ~args:["depth", `Int depth];
  )

let () =
  (* this just logs into a file. It's not thread safe nor multiprocess-safe. *)
  Catapult_file.with_setup @@ fun () ->
  let n = try int_of_string (Sys.getenv "N") with _ -> 10 in
  Printf.printf "run %d iterations\n%!" n;

  for _i = 1 to n do
    fake_trace 0;
  done
```

If run with the `TRACE=1` environment variable set, this will just produce a
basic trace in the file "trace.json" (otherwise probes will do nothing and keep
a minimal overhead).

Once opened in chrome://tracing, the trace looks like this:
![viewer screenshot](media/viewer1.png)

### Example: "heavy"

A more heavy example (used to benchmark a bit the tracing), is in `examples/heavy`.

In a terminal, run the daemon (if it's not already running):

``` 
$ ./daemon.sh
```

Then in another terminal:

```
$ ./heavy.sh -n=1 --mode=net -j 2
use net client tcp://127.0.0.1:6981
run 1 iterations
iteration 1
use net client tcp://127.0.0.1:6981
run 1 iterations
iteration 1

# list traces
$ catapult-conv
…
catapult-2022-2-16-16-36-18-pid-3229175.dbo

# convert last trace into a json.gz file
$ catapult-conv catapult-2022-2-16-16-36-18-pid-3229175.db

$ ls -lh trace.json.gz 
-rw-r--r-- 1 simon simon 374K Feb 16 11:38 trace.json.gz
```

Opened in chrome, the trace looks like that (focusing on a "step" event):
![viewer screenshot](media/viewer2.png)

## Coverage

- [x] duration events
- [x] async events
- [x] flow events
- [x] instants
- [x] metadata
- [x] counters
- [x] object events
- [ ] contexts
- [ ] memory dumps
- [ ] mark events
- [ ] clock synchro

## License

MIT
