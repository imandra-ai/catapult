(** Catapult-based tracing.

    A nice profiling format based on json, useful for visualizing what goes on.
    This library is the instrumentation part; see catapult-client, catapult-file,
    or use a custom {!BACKEND} to actually record traces.

    If a trace is obtained in, say, the file "trace.json.gz", it
    can be opened in
    chrome/chromium at "chrome://tracing".

    {{: https://github.com/wolfpld/tracy} Tracy} can import (uncompressed)
    trace files with a nice native trace explorer.

    See {{: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/}
          the documentation of TEF}
*)

module type BACKEND = Backend.S
module type IMPL = Impl.S

module Event_type = Event_type
module Arg = Arg
module Tracing = Tracing
module Nil_impl = Nil_impl
module Control = Tracing.Control
module Endpoint_address = Endpoint_address
module Ser = Ser

(**/**)

module Atomic_shim_ = Atomic_shim_
module Bare_encoding = Bare_encoding
module Clock = Clock

(**/**)
