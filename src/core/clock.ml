


let program_start = Mtime_clock.now()

(* main access to the clock *)
let[@inline] now_us () : float =
  let now = Mtime_clock.now() in
  Mtime.Span.to_us (Mtime.span program_start now)
