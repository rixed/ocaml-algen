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
	let one = 1 lsl Prec.v
	let add = (+)
	let neg s = -s
	exception Not_comparable
	let compare = compare
	let print ff s = Format.fprintf ff "%d.%d" (s asr Prec.v) (s land (one - 1))
	
	let mul a b =
		let m = Int64.shift_right_logical (Int64.mul (Int64.of_int a) (Int64.of_int b)) Prec.v in
		Int64.to_int m
	let of_float f = int_of_float (f *. (float_of_int one))
	let to_float s = (float_of_int s) /. (float_of_int one)
	exception No_root
	let sqrt s = of_float (sqrt (to_float s))

	exception Not_invertible
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_int a) Prec.v) (Int64.of_int b) in
		Int64.to_int m
	let inv s = div one s

	let rand = Random.int
end

(* Same as above, but using nativeints *)

module NatIntField (Prec : CONF_INT)
	: FIELD with type t = nativeint =
struct
	type t = nativeint
	let zero = 0n
	let one = Nativeint.shift_left 1n Prec.v
	let add = Nativeint.add
	let neg = Nativeint.neg
	exception Not_comparable
	let compare = Nativeint.compare
	let print ff s = Format.fprintf ff "%nd.%nd" (Nativeint.shift_right s Prec.v) (Nativeint.logand s (Nativeint.sub one 1n))

	let mul a b =
		let m = Int64.shift_right_logical (Int64.mul (Int64.of_nativeint a) (Int64.of_nativeint b)) Prec.v in
		Int64.to_nativeint m
	let of_float f = Nativeint.of_float (f *. (Nativeint.to_float one))
	let to_float s = (Nativeint.to_float s) /. (Nativeint.to_float one)
	exception No_root
	let sqrt s = of_float (sqrt (to_float s))

	exception Not_invertible
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_nativeint a) Prec.v) (Int64.of_nativeint b) in
		Int64.to_nativeint m
	let inv s = div one s

	let rand = Random.nativeint
end

