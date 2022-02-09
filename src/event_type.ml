
(** Event types. *)

type t =
  | B (** begin *)
  | E (** end *)
  | X (** begin+end *)
  | I (** instant *)
  | C (** counter*)
  | P (** sample *)
  | A_b (** async: begin 'b' *)
  | A_n (** async: snapshot 'n' *)
  | A_e (** async: end 'e' *)
  | F_s (** flow: start 's' *)
  | F_t (** flow: step 't' *)
  | F_f (** flow: end 'f' *)
  | N (** object: created *)
  | O (** object: shapshot *)
  | D (** object: destroyed *)
  | M (** metadata *)


