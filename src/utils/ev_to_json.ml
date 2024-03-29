module Out = Json_out

let[@inline] field_col oc = Out.char oc ':'
let[@inline] field_sep oc = Out.char oc ','
let any_val oc (j : string) = Out.raw_string oc j

(* emit [k:v] using printer [f] for the value *)
let field oc k f v : unit =
  Out.raw_string oc k;
  field_col oc;
  f oc v

let[@inline] opt_iter o f =
  match o with
  | None -> ()
  | Some x -> f x

let to_json buf (ev : Ser.Event.t) : string =
  let { Ser.Event.id; name; ph; pid; tid; cat; ts_us; args; stack; dur; extra }
      =
    ev
  in

  Buffer.clear buf;
  Out.char buf '{';

  field buf {|"name"|} Out.str_val name;
  field_sep buf;

  field buf {|"ph"|} Out.char_val (Char.chr ph);
  field_sep buf;

  field buf {|"tid"|} Out.int64 tid;
  field_sep buf;

  field buf {|"ts"|} Out.float ts_us;
  field_sep buf;

  opt_iter dur (fun dur ->
      field buf {|"dur"|} Out.float dur;
      field_sep buf);

  opt_iter id (fun i ->
      field buf {|"id"|} Out.str_val i;
      field_sep buf);

  opt_iter stack (fun s ->
      Out.raw_string buf {|"stack"|};
      field_col buf;
      Out.char buf '[';
      Array.iteri
        (fun i x ->
          if i > 0 then field_sep buf;
          any_val buf x)
        s;
      Out.char buf ']';
      field_sep buf);

  opt_iter cat (fun cs ->
      Out.raw_string buf {|"cat"|};
      field_col buf;
      Out.char buf '"';
      Array.iteri
        (fun i x ->
          if i > 0 then field_sep buf;
          Out.raw_string buf x)
        cs;
      Out.char buf '"';
      field_sep buf);

  opt_iter args (fun args ->
      Out.raw_string buf {|"args"|};
      field_col buf;
      Out.char buf '{';
      Array.iteri
        (fun i { Ser.Arg.key; value } ->
          if i > 0 then field_sep buf;
          Out.str_val buf key;
          field_col buf;
          match value with
          | Ser.Arg_value.Int64 i -> Out.int64 buf i
          | Ser.Arg_value.String s -> Out.str_val buf s
          | Ser.Arg_value.Float64 f -> Out.float buf f
          | Ser.Arg_value.Bool s -> Out.bool buf s
          | Ser.Arg_value.Void -> Out.null buf)
        args;
      Out.char buf '}';
      field_sep buf);

  opt_iter extra (fun l ->
      Array.iter
        (fun { Ser.Extra.key; value } ->
          Out.str_val buf key;
          field_col buf;
          Out.str_val buf value;
          field_sep buf)
        l);

  field buf {|"pid"|} Out.int64 pid;
  Out.char buf '}';
  Buffer.contents buf
