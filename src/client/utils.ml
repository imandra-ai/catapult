

let program_start = Mtime_clock.now()

let[@inline] now_ () : float =
  let now = Mtime_clock.now() in
  Mtime.Span.to_us (Mtime.span program_start now)
