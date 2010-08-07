open Algen_intf

(* Some FIELDs to play with. *)

module FloatField : FIELD with type t = float =
struct
	type t = float
	let zero = 0.
	let add = (+.)
	let neg x = -.x
	exception Not_comparable
	let compare = compare	(* From Pervasive *)
	let print ff s = Format.pp_print_float ff s
	
	let one = 1.
	let mul = ( *.)
	exception No_root
	let sqrt = sqrt

	exception Not_invertible
	let inv x = 1./.x

	let rand = Random.float
end

(* Fixed size precision based on int *)

module IntField (Prec : CONF_INT)
	: FIELD with type t = int (* FIXME: then the printer will be used for all ints :-< *)=
struct
	type t = int
	let zero = 0
	let add = (+)
	let neg s = -s
	exception Not_comparable
	let compare = compare
	let print ff s = Format.fprintf ff "%d.%d" (s asr Prec.v) (s land ((1 lsl Prec.v)-1))
	
	let one = 1 lsl Prec.v
	let mul a b =
		let m = Int64.shift_right_logical (Int64.mul (Int64.of_int a) (Int64.of_int b)) Prec.v in
		Int64.to_int m
	exception No_root
	let of_float f = int_of_float (f *. (float_of_int one))
	let to_float s = (float_of_int s) /. (float_of_int one)
	let sqrt s = of_float (sqrt (to_float s))

	exception Not_invertible
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_int a) Prec.v) (Int64.of_int b) in
		Int64.to_int m
	let inv s = div one s

	let rand = Random.int
end

