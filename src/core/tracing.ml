
module type BACKEND = Backend.S
type backend = (module BACKEND)

open Event_type

let now_ = Clock.now_us

type span_start = float
let null_span = neg_infinity

(* where to print events *)
let out_ : backend option ref = ref None

let[@inline] enabled() = !out_ != None

module Control = struct
  let setup b =
    assert (!out_ = None);
    out_ := b

  let teardown () =
    match !out_ with
    | None -> ()
    | Some (module B) ->
      out_ := None;
      B.teardown()
end

let pid = Unix.getpid()

type 'a emit_fun =
  ?cat:string list ->
  ?pid:int ->
  ?tid:int ->
  ?args:(string*Arg.t) list ->
  string ->
  'a

(* actually emit an event via the backend *)
let[@inline never] emit_real_
  (module B:BACKEND)
    ?ts_sec ?cat ?(pid=pid) ?(tid=Thread.self () |> Thread.id)
    ?stack ?args ?id ?extra ?dur name (ev:Event_type.t) : unit =
  let ts_sec = match ts_sec with Some x->x | None -> now_ () in
  B.emit
    ~id ~pid ~cat ~tid ~ts_sec ~stack ~args ~name ~ph:ev ~dur ?extra ();
  ()

let[@inline] emit ?cat ?pid ?tid ?args name (ev:Event_type.t) : unit =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args name ev

let[@inline] instant ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args name I

let[@inline] instant_with_stack ?cat ?pid ?tid ?args name ~stack =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args ~stack name I

let[@inline] counter ?cat ?pid ?tid ?(args=[]) name ~cs =
  match !out_ with
  | None -> ()
  | Some b ->
    let args = List.rev_append args @@ List.map (fun (k,v) -> k, `Int v) cs in
    emit_real_ b  ?cat ?pid ?tid name ~args C

let[@inline] meta ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid name ?args M

let meta_thread_name name =
  meta "thread_name" ~args:["name", `String name]

let meta_process_name name =
  meta "process_name" ~args:["name", `String name]

let[@inline] begin_ () : span_start =
  if !out_ == None then null_span
  else now_()

let exit_with_ b
     ?cat ?pid ?tid ?args ?stack name start : unit =
  let now = now_ () in
  let dur = now -. start in
  emit_real_
    b  ?cat ?pid ?tid ?args name ?stack
    ~ts_sec:start ~dur X

let[@inline] exit ?cat ?pid ?tid ?args name ?stack (sp:span_start) =
  if sp == null_span then ()
  else match !out_ with
  | None -> ()
  | Some b -> exit_with_ b  ?cat ?pid ?tid ?args name ?stack sp

let[@inline] with1 ?cat ?pid ?tid ?args name f x =
  match !out_ with
  | None -> f x
  | Some b ->
    let start = now_() in
    try
      let y = f x in
      exit_with_ b  ?cat ?pid ?tid name ?args start;
      y
    with e ->
      exit_with_ b  ?cat ?pid ?tid name ?args start;
      raise e

let[@inline] with_ ?cat ?pid ?tid ?args name f =
  with1 ?cat ?pid ?tid ?args name f ()

let[@inline] with2 ?cat ?pid ?tid ?args name f x y =
  with1  ?cat ?pid ?tid ?args name (fun () -> f x y) ()

let[@inline] with3 ?cat ?pid ?tid ?args name f x y z =
  with1  ?cat ?pid ?tid ?args name (fun () -> f x y z) ()

let[@inline] begin' ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name B

let[@inline] exit' ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name E

let[@inline] obj_new ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name ~id N

let[@inline] obj_delete ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name ~id D

let[@inline] obj_snap ?cat ?pid ?tid ?(args=[]) name ~snapshot ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid name
      ~args:(("snapshot", `String snapshot)::args) ~id O

let with1_gen_ ?cat ?pid ?tid ?args ?id name ev1 ev2 f x =
  match !out_ with
  | None -> f x
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args name ?id ev1;
    try
      let y = f x in
      (* exit: do not pass args *)
      emit_real_ b  ?cat ?pid ?tid name ?id ev2;
      y
    with e ->
      emit_real_ b  ?cat ?pid ?tid name ?id ev2;
      raise e

let[@inline] obj_with1 ?cat ?pid ?tid ?args name ~id f x =
  with1_gen_  ?cat ?pid ?tid ?args name ~id N D f x

let[@inline] obj_with ?cat ?pid ?tid ?args name ~id f =
  obj_with1  ?cat ?pid ?tid ?args name f () ~id

let[@inline] a_begin
     ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name ~id A_b

let[@inline] a_exit ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name ~id A_e

let[@inline] a_snap ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?cat ?pid ?tid ?args name A_n ~id

let[@inline] a_with1 ?cat ?pid ?tid ?args name ~id f x =
  with1_gen_  ?cat ?pid ?tid ?args name ~id A_b A_e f x

let[@inline] a_with ?cat ?pid ?tid ?args name ~id f =
  a_with1  ?cat ?pid ?tid ?args name f () ~id

let[@inline] f_begin ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args name ~id F_s

let[@inline] f_exit ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args name ~id F_f
      ~extra:["bp", "e"]

let[@inline] f_step ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?cat ?pid ?tid ?args name ~id F_t

let[@inline] tick () = match !out_ with
  | None -> ()
  | Some (module B) -> B.tick()

module Syntax = struct
  let (let@) x f = x f
end
include Syntax
