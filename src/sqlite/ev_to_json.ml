
module P = Catapult

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

let to_json buf (ev:P.Ser.Event.t) : string =
  let
    { P.Ser.Event.
      id; name; ph; pid; tid; cat; ts_sec; args; stack; dur; extra } = ev
  in

  Buffer.clear buf;
  Out.char buf '{';

  field buf {|"name"|} Out.str_val name;
  field_sep buf;

  field buf {|"ph"|} Out.char_val (Char.chr ph);
  field_sep buf;

  field buf {|"tid"|} Out.int64 tid;
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
      Array.iteri (fun i x -> if i>0 then field_sep buf; any_val buf x) s;
      Out.char buf ']';
      field_sep buf;
    );

  opt_iter cat (fun cs ->
      Out.string buf {|"cat"|};
      field_col buf;
      Out.char buf '"';
      Array.iteri (fun i x -> if i>0 then field_sep buf; Out.string buf x) cs;
      Out.char buf '"';
      field_sep buf;
    );

  opt_iter args (fun args ->
      Out.string buf {|"args"|};
      field_col buf;
      Out.char buf '{';
      Array.iteri (fun i {P.Ser.Arg.key; value} ->
          if i>0 then field_sep buf;
          Out.str_val buf key; field_col buf;
          match value with
            | P.Ser.Arg_value.Arg_value_0 i -> Out.int64 buf i
            | P.Ser.Arg_value.Arg_value_1 s -> Out.str_val buf s)
        args;
      Out.char buf '}';
      field_sep buf;
    );

  opt_iter extra (fun l ->
      Array.iter (fun {P.Ser.Extra.key; value} ->
          Out.str_val buf key; field_col buf; Out.str_val buf value;
          field_sep buf)
        l);

  field buf {|"pid"|} Out.int64 pid;
  Out.char buf '}';
  Buffer.contents buf
