
let[@inline] now_us () : float =
  let t = Unix.gettimeofday() in
  t *. 1e6
