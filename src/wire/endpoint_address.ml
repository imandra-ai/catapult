
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

let of_string s =
  try
    if str_prefix ~pre:"tcp://" s then (
      let s = String.sub s 6 (String.length s-6) in
      match String.index_opt s ':' with
      | Some i ->
        let s1 = String.sub s 0 i in
        let p = try int_of_string (String.sub s (i+1) (String.length s-i-1)) with _ -> raise Exit in
        Some (Tcp (s1,p))
      | None -> raise Exit
    ) else if str_prefix ~pre:"ipc://" s then (
      let s = String.sub s 6 (String.length s-6) in
      Some (Unix s)
    ) else None
  with Exit -> None

let default = Tcp ("127.0.0.1", 6981)
