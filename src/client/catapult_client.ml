module P = Catapult
module Endpoint_address = Catapult_utils.Endpoint_address

let trace_id = ref (try Sys.getenv "TRACE_ID" with _ -> "")
let set_trace_id s = trace_id := s

(* try to make a non-stupid default id, based on PID + date.
   This is not perfect, use a UUID4 if possible. *)
let[@inline never] invent_trace_id_ () : string =
  let pid = Unix.getpid () in
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  Printf.sprintf "catapult-%d-%02d-%02d-%02d-%02d-%02d-pid-%d"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min
    tm.tm_sec pid

let[@inline] get_trace_id () =
  if !trace_id = "" then trace_id := invent_trace_id_ ();
  !trace_id

let default_endpoint = Endpoint_address.default

let endpoint =
  ref
    (try Endpoint_address.of_string_exn (Sys.getenv "TRACE_ENDPOINT")
     with _ -> default_endpoint)

let set_endpoint e = endpoint := e
let get_endpoint () = !endpoint
let set_tcp_endpoint h p = set_endpoint (Endpoint_address.Tcp (h, p))
let set_ipc_endpoint file = set_endpoint (Endpoint_address.Unix file)
let tef_in_env () = List.mem (Sys.getenv_opt "TRACE") [ Some "1"; Some "true" ]

let mk_lazy_enable getenv =
  let r = ref false in
  let enabled_thunk = lazy (!r || getenv ()) in
  let[@inline] enabled () = Lazy.force enabled_thunk in
  let enable () = if not !r then r := true in
  enable, enabled

let enable, enabled = mk_lazy_enable tef_in_env

(* FIXME: with_ … *)
let setup_ =
  lazy
    (if enabled () then (
      at_exit P.Control.teardown;
      let trace_id = get_trace_id () in
      let conn = Connections.create ~addr:!endpoint ~trace_id () in
      let module B = Backend.Make (struct
        let conn = conn
      end) in
      let backend = (module B : P.BACKEND) in
      P.Control.setup (Some backend)
    ))

let setup () = Lazy.force setup_
let teardown = P.Tracing.Control.teardown

let with_setup f =
  setup ();
  try
    let x = f () in
    teardown ();
    x
  with e ->
    teardown ();
    raise e
