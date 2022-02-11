

module P = Catapult
module Tracing = P.Tracing

type event = P.Ser.Event.t

module type ARG = sig
  val writer : Writer.t
end

module Make(A : ARG) : P.BACKEND = struct
  let writer = A.writer

  let teardown () = Writer.close writer

  module Out = Json_buf_

  let[@inline] field_col oc = Out.char oc ':'
  let[@inline] field_sep oc = Out.char oc ','

  let any_val oc (j:string) = Out.string oc j

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
    let j =
      Writer.with_buf writer @@ fun buf ->

      Out.char buf '{';

      field buf {|"name"|} Out.str_val name;
      field_sep buf;

      field buf {|"ph"|} Out.char (P.Event_type.to_char ph);
      field_sep buf;

      field buf {|"tid"|} any_val (string_of_int tid);
      field_sep buf;

      field buf {|"ts"|} Out.float ts_sec;
      field_sep buf;

      opt_iter dur (fun dur ->
          field buf {|"dur"|} Out.float dur;
          field_sep buf;
        );

      opt_iter id (fun i ->
          field buf {|"id"|} Out.str_val i;
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
              Out.str_val buf k; field_col buf; Out.arg buf (v:P.Arg.t))
            args;
          Out.char buf '}';
          field_sep buf;
        );

      opt_iter extra (fun l ->
          List.iter (fun (x,y) ->
              Out.str_val buf x; field_col buf; Out.str_val buf y;
              field_sep buf)
            l);

      field buf {|"pid"|} Out.int pid;
      Out.char buf '}';
      Buffer.contents buf
    in
    Writer.write_string writer j;
    ()
end
