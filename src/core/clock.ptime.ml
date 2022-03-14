

let program_start = Ptime_clock.now()

(* main access to the clock *)
let[@inline] now_us () : float =
  let now = Ptime_clock.now() in
  Ptime.to_float_s now *. 1e6
