
type t =
  | Unix of string
  | Tcp of string * int

let to_string = function
  | Unix s -> Printf.sprintf "ipc://%s" s
  | Tcp (addr,port) -> Printf.sprintf "tcp://%s:%d" addr port

let str_prefix ~pre s =
  let len = String.length pre in
  if len > String.length s then false
  else (
    let rec check i =
      if i=len then true
      else if Stdlib.(<>) (String.unsafe_get s i) (String.unsafe_get pre i) then false
      else check (i+1)
    in
    check 0
  )

let of_string_exn s =
  if str_prefix ~pre:"tcp://" s then (
    let s = String.sub s 6 (String.length s-6) in
    match String.index_opt s ':' with
    | Some i ->
      let s1 = String.sub s 0 i in
      let p =
        try int_of_string (String.sub s (i+1) (String.length s-i-1))
        with _ -> invalid_arg "endpoint addr: invalid port" in
      Tcp (s1,p)
    | None -> invalid_arg "endpoint addr: invalid tcp address"
  ) else if str_prefix ~pre:"ipc://" s then (
    let s = String.sub s 6 (String.length s-6) in
    Unix s
  ) else (
    invalid_arg "endpoint addr: invalid address (expect tcp:// or ipc://)"
  )

let of_string s = try Some (of_string_exn s) with _ -> None

let default = Tcp ("127.0.0.1", 6981)
