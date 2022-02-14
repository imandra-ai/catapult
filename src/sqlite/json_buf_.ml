
let char = Buffer.add_char
let string = Buffer.add_string
let int out i = string out (string_of_int i)
let int64 out i = string out (Int64.to_string i)
let float out f = string out (Printf.sprintf "%.1f" f)
let bool out = function true -> string out "true" | false -> string out "false"
let null oc = string oc "null"

let str_val oc (s:string) =
  char oc '"';
  string oc (String.escaped s);
  char oc '"'

let arg oc = function
  | `Int i -> int oc i
  | `String s -> str_val oc s
  | `Bool b -> bool oc b
  | `Null -> null oc

let char_val oc (c:char) =
  char oc '"';
  char oc c;
  char oc '"'
