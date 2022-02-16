(* generated from "ser.bare" using bare-codegen *)
[@@@ocaml.warning "-26-27"]
module Bare = Bare_encoding
module Arg_value = struct
  type t =
    | Int64 of int64
    | String of string
    | Bool of bool
    | Float64 of float
    | Void
    
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> Int64 (Bare.Decode.i64 dec)
    | 1L -> String (Bare.Decode.string dec)
    | 2L -> Bool (Bare.Decode.bool dec)
    | 3L -> Float64 (Bare.Decode.f64 dec)
    | 4L -> Void
    | _ -> invalid_arg
      (Printf.sprintf "unknown union tag Arg_value.t: %Ld" tag)
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | Int64 x ->
      Bare.Encode.uint enc 0L;
      Bare.Encode.i64 enc x
    | String x ->
      Bare.Encode.uint enc 1L;
      Bare.Encode.string enc x
    | Bool x ->
      Bare.Encode.uint enc 2L;
      Bare.Encode.bool enc x
    | Float64 x ->
      Bare.Encode.uint enc 3L;
      Bare.Encode.f64 enc x
    | Void ->
      Bare.Encode.uint enc 4L
    
    
    let pp out (self:t) : unit =
      match self with
      | Int64 x ->
        Format.fprintf out "(@[Int64@ %a@])" Bare.Pp.int64 x
      | String x ->
        Format.fprintf out "(@[String@ %a@])" Bare.Pp.string x
      | Bool x ->
        Format.fprintf out "(@[Bool@ %a@])" Bare.Pp.bool x
      | Float64 x ->
        Format.fprintf out "(@[Float64@ %a@])" Bare.Pp.float x
      | Void ->
        Format.fprintf out "Void"
      
      
end

module Arg = struct
  type t = {
    key: string;
    value: Arg_value.t;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let key = Bare.Decode.string dec in
    let value = Arg_value.decode dec in
    {key; value; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.key;
      Arg_value.encode enc self.value;
    end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "key=%a;@ " Bare.Pp.string x.key;
       Format.fprintf out "value=%a;@ " Arg_value.pp x.value;
       Format.fprintf out "@]}";
     end) out self

end

module Extra = struct
  type t = {
    key: string;
    value: string;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let key = Bare.Decode.string dec in
    let value = Bare.Decode.string dec in
    {key; value; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.key;
      Bare.Encode.string enc self.value;
    end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "key=%a;@ " Bare.Pp.string x.key;
       Format.fprintf out "value=%a;@ " Bare.Pp.string x.value;
       Format.fprintf out "@]}";
     end) out self

end

module Event = struct
  type t = {
    id: string option;
    name: string;
    ph: int;
    pid: int64;
    tid: int64;
    cat: string array option;
    ts_us: float;
    args: Arg.t array option;
    stack: string array option;
    dur: float option;
    extra: Extra.t array option;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let id = Bare.Decode.optional (fun dec -> Bare.Decode.string dec) dec in
    let name = Bare.Decode.string dec in
    let ph = Bare.Decode.i16 dec in
    let pid = Bare.Decode.int dec in
    let tid = Bare.Decode.int dec in
    let cat =
      Bare.Decode.optional
        (fun dec ->
         (let len = Bare.Decode.uint dec in
          if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
          Array.init (Int64.to_int len) (fun _ -> Bare.Decode.string dec))) dec in
    let ts_us = Bare.Decode.f64 dec in
    let args =
      Bare.Decode.optional
        (fun dec ->
         (let len = Bare.Decode.uint dec in
          if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
          Array.init (Int64.to_int len) (fun _ -> Arg.decode dec))) dec in
    let stack =
      Bare.Decode.optional
        (fun dec ->
         (let len = Bare.Decode.uint dec in
          if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
          Array.init (Int64.to_int len) (fun _ -> Bare.Decode.string dec))) dec in
    let dur = Bare.Decode.optional (fun dec -> Bare.Decode.f64 dec) dec in
    let extra =
      Bare.Decode.optional
        (fun dec ->
         (let len = Bare.Decode.uint dec in
          if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
          Array.init (Int64.to_int len) (fun _ -> Extra.decode dec))) dec in
    {id; name; ph; pid; tid; cat; ts_us; args; stack; dur; extra; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.optional
        (fun enc xopt -> Bare.Encode.string enc xopt) enc self.id;
      Bare.Encode.string enc self.name;
      Bare.Encode.i16 enc self.ph;
      Bare.Encode.int enc self.pid;
      Bare.Encode.int enc self.tid;
      Bare.Encode.optional
        (fun enc xopt ->
         (let arr = xopt in
          Bare.Encode.uint enc (Int64.of_int (Array.length arr));
          Array.iter (fun xi -> Bare.Encode.string enc xi) arr)) enc self.cat;
      Bare.Encode.f64 enc self.ts_us;
      Bare.Encode.optional
        (fun enc xopt ->
         (let arr = xopt in
          Bare.Encode.uint enc (Int64.of_int (Array.length arr));
          Array.iter (fun xi -> Arg.encode enc xi) arr)) enc self.args;
      Bare.Encode.optional
        (fun enc xopt ->
         (let arr = xopt in
          Bare.Encode.uint enc (Int64.of_int (Array.length arr));
          Array.iter (fun xi -> Bare.Encode.string enc xi) arr)) enc self.stack;
      Bare.Encode.optional
        (fun enc xopt -> Bare.Encode.f64 enc xopt) enc self.dur;
      Bare.Encode.optional
        (fun enc xopt ->
         (let arr = xopt in
          Bare.Encode.uint enc (Int64.of_int (Array.length arr));
          Array.iter (fun xi -> Extra.encode enc xi) arr)) enc self.extra;
    end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "id=%a;@ " (Bare.Pp.option Bare.Pp.string) x.id;
       Format.fprintf out "name=%a;@ " Bare.Pp.string x.name;
       Format.fprintf out "ph=%a;@ " Bare.Pp.int x.ph;
       Format.fprintf out "pid=%a;@ " Bare.Pp.int64 x.pid;
       Format.fprintf out "tid=%a;@ " Bare.Pp.int64 x.tid;
       Format.fprintf out "cat=%a;@ "
         (Bare.Pp.option (Bare.Pp.array Bare.Pp.string)) x.cat;
       Format.fprintf out "ts_us=%a;@ " Bare.Pp.float x.ts_us;
       Format.fprintf out "args=%a;@ "
         (Bare.Pp.option (Bare.Pp.array Arg.pp)) x.args;
       Format.fprintf out "stack=%a;@ "
         (Bare.Pp.option (Bare.Pp.array Bare.Pp.string)) x.stack;
       Format.fprintf out "dur=%a;@ " (Bare.Pp.option Bare.Pp.float) x.dur;
       Format.fprintf out "extra=%a;@ "
         (Bare.Pp.option (Bare.Pp.array Extra.pp)) x.extra;
       Format.fprintf out "@]}";
     end) out self

end

module Client_open_trace = struct
  type t = {
    trace_id: string;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let trace_id = Bare.Decode.string dec in {trace_id; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin Bare.Encode.string enc self.trace_id; end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "trace_id=%a;@ " Bare.Pp.string x.trace_id;
       Format.fprintf out "@]}";
     end) out self

end

module Client_close_trace = struct
  type t = {
    trace_id: string;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let trace_id = Bare.Decode.string dec in {trace_id; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin Bare.Encode.string enc self.trace_id; end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "trace_id=%a;@ " Bare.Pp.string x.trace_id;
       Format.fprintf out "@]}";
     end) out self

end

module Client_emit = struct
  type t = {
    trace_id: string;
    ev: Event.t;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let trace_id = Bare.Decode.string dec in
    let ev = Event.decode dec in
    {trace_id; ev; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin Bare.Encode.string enc self.trace_id; Event.encode enc self.ev; end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "trace_id=%a;@ " Bare.Pp.string x.trace_id;
       Format.fprintf out "ev=%a;@ " Event.pp x.ev;
       Format.fprintf out "@]}";
     end) out self

end

module Client_message = struct
  type t =
    | Client_open_trace of Client_open_trace.t
    | Client_close_trace of Client_close_trace.t
    | Client_emit of Client_emit.t
    
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> Client_open_trace (Client_open_trace.decode dec)
    | 1L -> Client_close_trace (Client_close_trace.decode dec)
    | 2L -> Client_emit (Client_emit.decode dec)
    | _ -> invalid_arg
      (Printf.sprintf "unknown union tag Client_message.t: %Ld" tag)
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | Client_open_trace x ->
      Bare.Encode.uint enc 0L;
      Client_open_trace.encode enc x
    | Client_close_trace x ->
      Bare.Encode.uint enc 1L;
      Client_close_trace.encode enc x
    | Client_emit x ->
      Bare.Encode.uint enc 2L;
      Client_emit.encode enc x
    
    
    let pp out (self:t) : unit =
      match self with
      | Client_open_trace x ->
        Format.fprintf out "(@[Client_open_trace@ %a@])" Client_open_trace.pp x
      | Client_close_trace x ->
        Format.fprintf out "(@[Client_close_trace@ %a@])" Client_close_trace.pp x
      | Client_emit x ->
        Format.fprintf out "(@[Client_emit@ %a@])" Client_emit.pp x
      
      
end


