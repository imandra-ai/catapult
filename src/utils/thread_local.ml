module P = Catapult
module Atomic = P.Atomic_shim_

(* emulate thread local storage *)
module Int_map = Map.Make (struct
  type t = int

  let compare : int -> int -> int = compare
end)

type 'a value = { value: 'a } [@@unboxed]

type 'a t = {
  maps: 'a value Int_map.t Atomic.t array;
  init: t_id:int -> 'a;
  close: 'a -> unit;
}
(** A thread-local map. *)

let[@inline] modify_map_ ~f (self : _ t) i =
  let map = self.maps.(i) in
  while
    let cur = Atomic.get map in
    let new_ = f cur in
    not (Atomic.compare_and_set map cur new_)
  do
    ()
  done

let size_map m = Int_map.cardinal (Atomic.get m)

let size self =
  let n = ref 0 in
  Array.iter (fun m -> n := !n + size_map m) self.maps;
  !n

let mask_ = 0b1111
let n_bits_ = 4

let remove (self : _ t) ~t_id =
  let offset = t_id land mask_ in
  let key = t_id lsl n_bits_ in
  let m = Atomic.get self.maps.(offset) in
  match Int_map.find_opt key m with
  | None -> ()
  | Some value ->
    modify_map_ self offset ~f:(fun m -> Int_map.remove key m);
    self.close value.value

let create_ self t t_id =
  let v = { value = self.init ~t_id } in
  let offset = t_id land mask_ in
  let key = t_id lsr n_bits_ in
  modify_map_ self offset ~f:(fun m -> Int_map.add key v m);
  Gc.finalise (fun _ -> remove self ~t_id) t;
  v.value

let get_or_create self : 'a =
  let t = Thread.self () in
  let t_id = Thread.id t in
  let offset = t_id land mask_ in
  let key = t_id lsr n_bits_ in
  let m = Atomic.get self.maps.(offset) in
  match Int_map.find_opt key m with
  | Some v -> v.value
  | None -> create_ self t t_id

let iter ~f self =
  for i = 0 to mask_ do
    let m = Atomic.get self.maps.(i) in
    Int_map.iter (fun _ v -> f v.value) m
  done

let clear self =
  for i = 0 to mask_ do
    let m = Atomic.exchange self.maps.(i) Int_map.empty in
    Int_map.iter (fun _ v -> self.close v.value) m
  done

let create ~init ~close () : _ t =
  let n_maps = 1 lsl n_bits_ in
  let m =
    {
      maps = Array.init n_maps (fun _ -> Atomic.make Int_map.empty);
      init;
      close;
    }
  in
  Gc.finalise clear m;
  m
