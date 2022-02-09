
module P = Catapult
module Tracing = P.Tracing

module Int_map = Map.Make(struct
    type t = int
    let compare=compare
  end)

type json = Yojson.Basic.json

let program_start = Mtime_clock.now()

let[@inline] now_ () : float =
  let now = Mtime_clock.now() in
  Mtime.Span.to_us (Mtime.span program_start now)


module State : sig
  type t_logger
  type t
  val create : filename:string -> t
  val get_logger : t -> t_id:int -> t_logger
  val send_msg : t_logger -> pid:int -> now:float -> (Buffer.t -> unit) -> unit
  val close : t -> unit
end = struct
  type t_logger = {
    t_main: t;
    t_id: int; (* thread id *)
    t_buf: Buffer.t; (* used to print a json object *)
    mutable t_active: bool; (* are we emitting an event already? *)
  }

  and t = {
    mutable closed: bool;
    mutable first: bool;
    oc: out_channel;
    lock: Mutex.t;
    mutable per_t: t_logger Int_map.t;
    mutable last_gc_stat: float; (* timestamp of last gc event *)
  }

  let[@inline] with_lock_ self f =
    Mutex.lock self.lock;
    try
      let x=f() in
      Mutex.unlock self.lock;
      x
    with e ->
      Mutex.unlock self.lock;
      raise e

  let close (self:t) =
    with_lock_ self @@ fun () ->
    if not self.closed then (
      self.closed <- true;
      output_char self.oc ']';
      flush self.oc;
      close_out self.oc;
    )

  let create ~(filename:string) : t =
    let oc = open_out_bin filename in
    let self = {
      oc; closed=false; first=true; per_t=Int_map.empty;
      lock=Mutex.create(); last_gc_stat=now_();
    } in
    Gc.finalise close self;
    self

  let emit_buf_ self logger : unit =
    with_lock_ self @@ fun () ->
    if self.first then (
      self.first <- false;
      output_char self.oc '[';
    ) else (
      output_string self.oc ",\n";
    );
    Buffer.output_buffer self.oc logger.t_buf

  let[@inline never] add_logger_ self ~t_id =
    let pt = {
      t_id; t_buf=Buffer.create 128; t_main=self; t_active=false;
    } in
    with_lock_ self
      (fun () -> self.per_t <- Int_map.add t_id pt self.per_t);
    pt

  (* obtain logger for this thread *)
  let[@inline] get_logger self ~t_id : t_logger =
    let m = self.per_t in
    match Int_map.find t_id m with
    | log -> log
    | exception Not_found -> add_logger_ self ~t_id

  (* emit a GC counter event *)
  let emit_gc_ ~pid () =
    let st = Gc.quick_stat() in
    Tracing.counter "gc" ~cs:[
      (Printf.sprintf "%d.major" pid), st.Gc.major_collections;
      (Printf.sprintf "%d.minor" pid), st.Gc.minor_collections;
      (Printf.sprintf "%d.compactions" pid), st.Gc.compactions;
      (Printf.sprintf "%d.heap_words" pid), st.Gc.heap_words;
      (Printf.sprintf "%d.heap_MB" pid), (st.Gc.heap_words * (Sys.word_size / 8) / 1024 / 1024);
      (Printf.sprintf "%d.minor_words" pid), (int_of_float st.Gc.minor_words);
    ]

  let send_msg (pt:t_logger) ~pid ~now (f:Buffer.t -> unit) : unit =
    let self = pt.t_main in
    begin
      let old_active = pt.t_active in
      let must_emit_gc_ = not pt.t_active && now -. self.last_gc_stat > 0.1 in
      pt.t_active <- true;

      (* time to emit some GC counters *)
      if must_emit_gc_ then (
        self.last_gc_stat <- now;
        emit_gc_ ~pid ();
      );

      let buf = pt.t_buf in
      Buffer.clear buf;
      f buf;
      assert (pt.t_id = Thread.id (Thread.self()));
      emit_buf_ self pt;

      pt.t_active <- old_active;
    end
end

module Backend() : P.BACKEND = struct
  let get_ts = now_

  let state = State.create ~filename:"trace.json"
  let teardown () = State.close state

  module Out = struct
    type t = Buffer.t
    let char = Buffer.add_char
    let string = Buffer.add_string
    let int out i = string out (string_of_int i)
  end

  (* already json-escaped *)
  let string_of_event_type =
    let open P.Event_type in
    function
    | B -> {|"B"|}
    | E -> {|"E"|}
    | X -> {|"X"|}
    | I -> {|"I"|}
    | C -> {|"C"|}
    | P -> {|"P"|}
    | A_b -> {|"b"|}
    | A_n -> {|"n"|}
    | A_e -> {|"e"|}
    | F_s -> {|"s"|}
    | F_t -> {|"t"|}
    | F_f -> {|"f"|}
    | N -> {|"N"|}
    | O -> {|"O"|}
    | D -> {|"D"|}
    | M -> {|"M"|}

  let[@inline] field_col oc = Out.char oc ':'
  let[@inline] field_sep oc = Out.char oc ','

  let json oc (j:json) = Out.string oc (Yojson.Basic.to_string j)
  let any_val oc (j:string) = Out.string oc j

  let str_val oc (s:string) =
    Out.char oc '"';
    let s = if String.contains s '"' then String.escaped s else s in
    Out.string oc s;
    Out.char oc '"'

  (* emit [k:v] using printer [f] for the value *)
  let field oc k f v : unit =
    Out.string oc k;
    field_col oc;
    f oc v

  let[@inline] opt_iter o f = match o with
    | None -> ()
    | Some x -> f x

  let emit
      ~id ~name ~ph ~tid ~pid ~cat ~ts_sec ~args ~stack ~dur ?extra () : unit =
    (* delegate to {!State} the task of allocating a buffer, and producing
       output. We just provide a callback that, given the buffer,
       writes the JSON into it. *)
    let logger = State.get_logger state ~t_id:(Thread.id (Thread.self())) in
    State.send_msg logger ~pid ~now:ts_sec @@ fun buf ->

    Out.char buf '{';

    field buf {|"name"|} str_val name;
    field_sep buf;

    field buf {|"ph"|} any_val (string_of_event_type ph);
    field_sep buf;

    field buf {|"tid"|} any_val (string_of_int tid);
    field_sep buf;

    field buf {|"ts"|} any_val (Printf.sprintf "%.1f" ts_sec);
    field_sep buf;

    opt_iter dur (fun dur ->
        field buf {|"dur"|} any_val (Printf.sprintf "%.1f" dur);
        field_sep buf;
      );

    opt_iter id (fun i ->
        field buf {|"id"|} str_val i;
        field_sep buf;
      );

    opt_iter stack (fun s ->
        Out.string buf {|"stack"|};
        field_col buf;
        Out.char buf '[';
        List.iteri (fun i x -> if i>0 then field_sep buf; any_val buf x) s;
        Out.char buf ']';
        field_sep buf;
      );

    opt_iter cat (fun cs ->
        Out.string buf {|"cat"|};
        field_col buf;
        Out.char buf '"';
        List.iteri (fun i x -> if i>0 then field_sep buf; Out.string buf x) cs;
        Out.char buf '"';
        field_sep buf;
      );

    opt_iter args (fun args ->
        Out.string buf {|"args"|};
        field_col buf;
        Out.char buf '{';
        List.iteri (fun i (k,v) ->
            if i>0 then field_sep buf;
            str_val buf k; field_col buf; json buf (v:P.Arg.t:>json))
          args;
        Out.char buf '}';
        field_sep buf;
      );

    opt_iter extra (fun l ->
        List.iter (fun (x,y) ->
            str_val buf x; field_col buf; str_val buf y;
            field_sep buf)
          l);

    field buf {|"pid"|} any_val (string_of_int pid);
    Out.char buf '}';
    ()
end

let tef_in_env() = List.mem (Sys.getenv_opt "TRACE") [Some"1";Some"true"]

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

let enable, enabled = mk_lazy_enable tef_in_env

let setup_ = lazy (
  if enabled() then (
    at_exit P.Control.teardown;
    let module B = Backend() in
    let backend = (module B : P.BACKEND) in
    P.Control.setup (Some backend);
  )
)

let setup () = Lazy.force setup_
let teardown = P.Tracing.Control.teardown

let[@inline] with_setup f =
  setup();
  try let x = f() in teardown(); x
  with e -> teardown(); raise e