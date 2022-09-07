(** Basic output of json values to a buffer *)

let char = Buffer.add_char
let raw_string = Buffer.add_string
let int out i = raw_string out (string_of_int i)
let int64 out i = raw_string out (Int64.to_string i)
let float out f = raw_string out (Printf.sprintf "%.1f" f)

let bool out = function
  | true -> raw_string out "true"
  | false -> raw_string out "false"

let null oc = raw_string oc "null"

let str_val oc (s : string) =
  char oc '"';
  let encode_char c =
    match c with
    | '"' -> raw_string oc {|\"|}
    | '\\' -> raw_string oc {|\\|}
    | '\n' -> raw_string oc {|\n|}
    | '\b' -> raw_string oc {|\b|}
    | '\r' -> raw_string oc {|\r|}
    | '\t' -> raw_string oc {|\t|}
    | _ when Char.code c <= 0x1f ->
      raw_string oc {|\u00|};
      Printf.bprintf oc "%02x" (Char.code c)
    | c -> char oc c
  in
  String.iter encode_char s;
  char oc '"'

let arg oc = function
  | `Int i -> int oc i
  | `String s -> str_val oc s
  | `Bool b -> bool oc b
  | `Float f -> float oc f
  | `Null -> null oc

let char_val oc (c : char) =
  char oc '"';
  char oc c;
  char oc '"'
