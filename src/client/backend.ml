open Catapult_utils
module P = Catapult

type event = Ser.Event.t

module type ARG = sig
  val conn : Connection.t
end

module Make (A : ARG) : P.BACKEND = struct
  let conn = A.conn
  let teardown () = Connection.close conn

  let[@inline] opt_map_ f = function
    | None -> None
    | Some x -> Some (f x)

  let conv_arg (key, (a : [< `Float of float | Trace.user_data ])) =
    let open Ser in
    let value =
      match a with
      | `Int x -> Arg_value.Int64 (Int64.of_int x)
      | `String s -> Arg_value.String s
      | `Float f -> Arg_value.Float64 f
      | `Bool b -> Arg_value.Bool b
      | `None -> Arg_value.Void
    in
    { Arg.key; value }

  let emit ~id ~name ~ph ~tid ~pid ~cat ~ts_us ~args ~stack ~dur ?extra () :
      unit =
    let ev =
      let open Ser in
      let tid = Int64.of_int tid in
      let pid = Int64.of_int pid in
      let stack = opt_map_ Array.of_list stack in
      let ph = P.Event_type.to_char ph |> Char.code in
      let cat = opt_map_ Array.of_list cat in
      let extra =
        match extra with
        | None -> None
        | Some l ->
          Some
            (Array.of_list l
            |> Array.map (fun (key, value) -> { Extra.key; value }))
      in
      let args =
        opt_map_ (fun l -> l |> Array.of_list |> Array.map conv_arg) args
      in
      { Event.id; name; ph; tid; pid; cat; ts_us; args; stack; dur; extra }
    in
    Connection.send_msg conn ~pid ~now:ts_us ev

  let tick () =
    let now = P.Clock.now_us () in
    let pid = Unix.getpid () in
    Gc_stats.maybe_emit ~now ~pid ()
end

let make (c : Connection.t) : P.backend =
  let module M = Make (struct
    let conn = c
  end) in
  (module M)
