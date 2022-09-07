(** Event types. *)

type t =
  | B  (** begin *)
  | E  (** end *)
  | X  (** begin+end *)
  | I  (** instant *)
  | C  (** counter*)
  | P  (** sample *)
  | A_b  (** async: begin 'b' *)
  | A_n  (** async: snapshot 'n' *)
  | A_e  (** async: end 'e' *)
  | F_s  (** flow: start 's' *)
  | F_t  (** flow: step 't' *)
  | F_f  (** flow: end 'f' *)
  | N  (** object: created *)
  | O  (** object: shapshot *)
  | D  (** object: destroyed *)
  | M  (** metadata *)

let to_char = function
  | B -> 'B'
  | E -> 'E'
  | X -> 'X'
  | I -> 'I'
  | C -> 'C'
  | P -> 'P'
  | A_b -> 'b'
  | A_n -> 'n'
  | A_e -> 'e'
  | F_s -> 's'
  | F_t -> 't'
  | F_f -> 'f'
  | N -> 'N'
  | O -> 'O'
  | D -> 'D'
  | M -> 'M'

let of_char = function
  | 'B' -> B
  | 'E' -> E
  | 'X' -> X
  | 'I' -> I
  | 'C' -> C
  | 'P' -> P
  | 'b' -> A_b
  | 'n' -> A_n
  | 'e' -> A_e
  | 's' -> F_s
  | 't' -> F_t
  | 'f' -> F_f
  | 'N' -> N
  | 'O' -> O
  | 'D' -> D
  | 'M' -> M
  | c -> invalid_arg (Printf.sprintf "catapult: unknown event type %C" c)
