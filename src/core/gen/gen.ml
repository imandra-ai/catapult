let shims_atomic_before_412 =
  {|
  type 'a t = {mutable x: 'a}
  let[@inline] make x = {x}
  let[@inline] get {x} = x
  let[@inline] set r x = r.x <- x
  let[@inline] exchange r x =
    let y = r.x in
    r.x <- x;
    y

  let[@inline] compare_and_set r seen v =
    if r.x == seen then (
      r.x <- v;
      true
    ) else false

  let[@inline] fetch_and_add r x =
    let v = r.x in
    r.x <- x + r.x;
    v

  let[@inline] incr r = r.x <- 1 + r.x
  let[@inline] decr r = r.x <- r.x - 1
  |}

let shims_atomic_after_412 = {|include Atomic|}

let () =
  let maj, min =
    Scanf.sscanf Sys.ocaml_version "%d.%d.%s" (fun x y _ -> x, y)
  in
  print_endline
    (if (maj, min) >= (4, 12) then
      shims_atomic_after_412
    else
      shims_atomic_before_412);
  ()
