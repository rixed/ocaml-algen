open Algen_intf

(* Some FIELDs to play with. *)

module FloatField : FIELD with type t = float

module IntField (Prec : CONF_INT)
	: FIELD with type t = int

