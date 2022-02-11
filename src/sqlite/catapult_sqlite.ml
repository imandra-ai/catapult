

module P = Catapult
module Tracing = P.Tracing

module Backend = Backend
module Writer = Writer
module Ev_to_json = Ev_to_json

let trace_id = ref (try Sys.getenv "TRACE_ID" with _ -> "")
let set_trace_id s = trace_id := s

let file = ref (try Sys.getenv "TRACE_DB" with _ -> "")
let set_file f = file := f

let sqlite_sync_ = ref None
let set_sqlite_sync s = sqlite_sync_ := Some s

(* try to make a non-stupid default id, based on PID + date.
   This is not perfect, use a UUID4 if possible. *)
let[@inline never] invent_trace_id_ () : string =
  let pid = Unix.getpid() in
  let now = Unix.gettimeofday() in
  let tm = Unix.gmtime now in
  Printf.sprintf "catapult-%d-%d-%0d-%02d-%02d-%02d-pid-%d"
    (1900+tm.tm_year) (tm.tm_mon+1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec pid

let[@inline] get_trace_id () =
  if !trace_id="" then trace_id := invent_trace_id_();
  !trace_id

let trace_in_env() = List.mem (Sys.getenv_opt "TRACE") [Some"1";Some"true"]

let mk_lazy_enable getenv =
  let r = ref false in
  let enabled_thunk = lazy (
    !r || getenv()
  ) in
  let[@inline] enabled() = Lazy.force enabled_thunk in
  let enable () =
    if not !r then (
      r := true;
    )
  in
  enable, enabled

let enable, enabled = mk_lazy_enable trace_in_env

module Dir = Directories.Project_dirs(struct
    let qualifier = "ai"
    let organization = "imandra"
    let application = "catapult"
  end)

let dir = ref @@ match Dir.data_dir with None -> "." | Some d -> d
let set_dir d = dir := d

let setup_ = lazy (
  if enabled() then (
    at_exit P.Control.teardown;
    let trace_id = get_trace_id() in
    let file = if !file="" then None else Some !file in
    let writer = Writer.create ?sync:!sqlite_sync_ ?file ~trace_id ~dir:!dir () in
    let module B = Backend.Make(struct
        let writer = writer
      end) in
    let backend = (module B : P.BACKEND) in
    P.Control.setup (Some backend);
  )
)

let setup () = Lazy.force setup_
let teardown = P.Tracing.Control.teardown

let with_setup f =
  setup();
  try let x = f() in teardown(); x
  with e -> teardown(); raise e
