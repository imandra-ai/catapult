
module P = Catapult
module Atomic = P.Atomic_shim_

(* emulate thread local storage *)
module Int_map = Map.Make(struct
    type t = int
    let compare: int -> int -> int = compare
  end)

type 'a value = {
  t_id: int; (* thread id *)
  value: 'a;
}

(** A thread-local map. *)
type 'a t = {
  map: 'a value Int_map.t Atomic.t;
  init: t_id:int -> 'a;
  close: 'a -> unit;
}

let[@inline] modify_map_ ~f (self:_ t) =
  while not (
    let cur = Atomic.get self.map in
    let new_ = f cur in
    Atomic.compare_and_set self.map cur new_
  )
  do () done

let get_or_create self ~t_id : 'a =
  let m = Atomic.get self.map in
  match Int_map.find_opt t_id m with
  | Some v -> v.value
  | None ->
    let v = {t_id; value=self.init ~t_id} in
    modify_map_ self ~f:(fun m -> Int_map.add t_id v m);
    v.value

let remove (self:_ t) ~t_id =
  let m = Atomic.get self.map in
  match Int_map.find_opt t_id m with
  | None -> ()
  | Some value ->
    modify_map_ self ~f:(fun m -> Int_map.remove t_id m);
    self.close value.value

let clear self =
  let m = Atomic.exchange self.map Int_map.empty in
  Int_map.iter (fun _ v -> self.close v.value) m

let create ~init ~close () : _ t =
  let m = { map=Atomic.make Int_map.empty; init; close; } in
  Gc.finalise clear m;
  m
