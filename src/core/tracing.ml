
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

type arg = [`Int of int | `String of string | `Float of float | `Bool of bool | `Null]

let () =
  (* make sure Arg.t = arg *)
  let _check_cast : Arg.t -> arg = (fun x->x) in
  ()

type 'a emit_fun_base =
  ?cat:string list ->
  ?pid:int ->
  ?tid:int ->
  ?args:(string*arg) list ->
  string ->
  'a

type 'a emit_fun = ?ts_us:float -> 'a emit_fun_base
type 'a with_stack = ?stack:string list -> 'a

(* actually emit an event via the backend *)
let[@inline never] emit_real_
  (module B:BACKEND)
    ?ts_us ?cat ?(pid=pid) ?(tid=Thread.self () |> Thread.id)
    ?stack ?args ?id ?extra ?dur name (ev:Event_type.t) : unit =
  let ts_us = match ts_us with Some x->x | None -> now_ () in
  B.emit
    ~id ~pid ~cat ~tid ~ts_us ~stack ~args ~name ~ph:ev ~dur ?extra ();
  ()

let[@inline] emit ?stack ?ts_us ?cat ?pid ?tid ?args name ?dur (ev:Event_type.t) : unit =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args ?dur name ev

let[@inline] instant ?stack ?ts_us ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args name I

let[@inline] counter ?ts_us ?cat ?pid ?tid ?(args=[]) name ~cs =
  match !out_ with
  | None -> ()
  | Some b ->
    let args = List.rev_append args @@ List.map (fun (k,v) -> k, `Int v) cs in
    emit_real_ b  ?ts_us ?cat ?pid ?tid name ~args C

let[@inline] meta ?ts_us ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?ts_us ?cat ?pid ?tid name ?args M

let meta_thread_name name =
  meta "thread_name" ~args:["name", `String name]

let meta_process_name name =
  meta "process_name" ~args:["name", `String name]

let[@inline] begin_ () : span_start =
  if !out_ == None then null_span
  else now_()

let exit_with_ b ?stack ?cat ?pid ?tid ?args name start : unit =
  let now = now_ () in
  let dur = now -. start in
  emit_real_
    b ?cat ?pid ?tid ?args name ?stack
    ~ts_us:start ~dur X

let[@inline] exit ?stack ?cat ?pid ?tid ?args name (sp:span_start) =
  if sp == null_span then ()
  else match !out_ with
  | None -> ()
  | Some b -> exit_with_ b ?stack ?cat ?pid ?tid ?args name sp

let[@inline] with1 ?cat ?pid ?tid ?args name f x =
  match !out_ with
  | None -> f x
  | Some b ->
    let start = now_() in
    try
      let y = f x in
      exit_with_ b ?cat ?pid ?tid name ?args start;
      y
    with e ->
      exit_with_ b ?cat ?pid ?tid name ?args start;
      raise e

let[@inline] with_ ?cat ?pid ?tid ?args name f =
  with1 ?cat ?pid ?tid ?args name f ()

let[@inline] with2 ?cat ?pid ?tid ?args name f x y =
  with1  ?cat ?pid ?tid ?args name (fun () -> f x y) ()

let[@inline] with3 ?cat ?pid ?tid ?args name f x y z =
  with1  ?cat ?pid ?tid ?args name (fun () -> f x y z) ()

let[@inline] begin' ?stack ?ts_us ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args name B

let[@inline] exit' ?stack ?ts_us ?cat ?pid ?tid ?args name =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args name E

let[@inline] span ?stack ?ts_us ?cat ?pid ?tid ?args name ~dur =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args ~dur name X

let[@inline] obj_new ?stack ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args name ~id N

let[@inline] obj_delete ?stack ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b ?stack ?ts_us ?cat ?pid ?tid ?args name ~id D

let[@inline] obj_snap ?stack ?ts_us ?cat ?pid ?tid ?(args=[]) name ~snapshot ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b ?stack ?ts_us ?cat ?pid ?tid name
      ~args:(("snapshot", `String snapshot)::args) ~id O

let with1_gen_ ?ts_us ?cat ?pid ?tid ?args ?id name ev1 ev2 f x =
  match !out_ with
  | None -> f x
  | Some b ->
    emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name ?id ev1;
    try
      let y = f x in
      (* exit: do not pass args *)
      emit_real_ b  ?ts_us ?cat ?pid ?tid name ?id ev2;
      y
    with e ->
      emit_real_ b  ?ts_us ?cat ?pid ?tid name ?id ev2;
      raise e

let[@inline] obj_with1 ?ts_us ?cat ?pid ?tid ?args name ~id f x =
  with1_gen_  ?ts_us ?cat ?pid ?tid ?args name ~id N D f x

let[@inline] obj_with ?ts_us ?cat ?pid ?tid ?args name ~id f =
  obj_with1  ?ts_us ?cat ?pid ?tid ?args name f () ~id

let[@inline] a_begin
     ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name ~id A_b

let[@inline] a_exit ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name ~id A_e

let[@inline] a_snap ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b -> emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name A_n ~id

let[@inline] a_with1 ?ts_us ?cat ?pid ?tid ?args name ~id f x =
  with1_gen_  ?ts_us ?cat ?pid ?tid ?args name ~id A_b A_e f x

let[@inline] a_with ?ts_us ?cat ?pid ?tid ?args name ~id f =
  a_with1  ?ts_us ?cat ?pid ?tid ?args name f () ~id

let[@inline] f_begin ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name ~id F_s

let[@inline] f_exit ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name ~id F_f
      ~extra:["bp", "e"]

let[@inline] f_step ?ts_us ?cat ?pid ?tid ?args name ~id =
  match !out_ with
  | None -> ()
  | Some b ->
    emit_real_ b  ?ts_us ?cat ?pid ?tid ?args name ~id F_t

let[@inline] tick () = match !out_ with
  | None -> ()
  | Some (module B) -> B.tick()

module Syntax = struct
  let (let@) x f = x f
end
include Syntax
