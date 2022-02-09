# Catapult [![build](https://github.com/AestheticIntegration/catapult/actions/workflows/main.yml/badge.svg)](https://github.com/AestheticIntegration/catapult/actions/workflows/main.yml)

This is a tracing library for OCaml, based on the [Catapult/TEF] trace format.

[Catapult/TEF](https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/)

The traces are `.json` files (or compressed `.json.gz`). They can be viewed in:
- https://ui.perfetto.dev/
- chrome://tracing in chrome/chromium
- https://github.com/wolfpld/tracy after conversion (the `tracy-import-chrome` binary)

## Usage

The core library is `catapult`. It's a small set of probes that can be
inserted in your code, by hand (with meaningful messages if needed).

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


## License

MIT
