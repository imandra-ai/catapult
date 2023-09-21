module TL = Catapult.Thread_local

let tl : int ref TL.t =
  TL.create ~init:(fun ~t_id:_ -> ref 0) ~close:(fun _ -> ()) ()

let () =
  Printf.printf "start\n%!";
  let l = ref [] in
  let mutex = Mutex.create () in

  let run () =
    for i = 1 to 1_000_000 do
      let r = TL.get_or_create tl in
      incr r
    done;
    let r = TL.get_or_create tl in
    Mutex.lock mutex;
    l := !r :: !l;
    Mutex.unlock mutex
  in

  let threads = Array.init 90 (fun _ -> Thread.create run ()) in
  Array.iter Thread.join threads;

  let threads = Array.init 50 (fun _ -> Thread.create run ()) in
  Array.iter Thread.join threads;

  Printf.printf "stop (l: [%s])\n%!"
    (String.concat "; " @@ List.map string_of_int !l);
  assert (!l = List.init (50 + 90) (fun _ -> 1_000_000));
  ()
