open Algen_intf

(* Some FIELDs to play with. *)

module FloatField :
	FIELD with type t = float =
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

	exception Not_convertible
	let of_int = float_of_int
	let to_int = int_of_float
	let of_float x = x
	let to_float x = x
	let of_nativeint = Nativeint.to_float
	let to_nativeint = Nativeint.of_float
	let of_int64 = Int64.to_float
	let to_int64 = Int64.of_float
end

(* Fixed size precision based on int *)

module IntField (Prec : CONF_INT)
	: FIELD with type t = int =
struct
	type t = int
	let zero = 0
	let one = 1 lsl Prec.v
	let add = (+)
	let neg s = -s
	exception Not_comparable
	let compare = compare
	let of_float f = int_of_float (f *. (float_of_int one))
	let to_float s = (float_of_int s) /. (float_of_int one)
	let print ff s = Format.fprintf ff "%g" (to_float s)
	
	let mul a b =
		let m = Int64.shift_right (Int64.mul (Int64.of_int a) (Int64.of_int b)) Prec.v in
		Int64.to_int m
	exception No_root
	let sqrt s = of_float (sqrt (to_float s))

	exception Not_invertible
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_int a) Prec.v) (Int64.of_int b) in
		Int64.to_int m
	let inv s = div one s

	let rand = Random.int

	exception Not_convertible
	let of_int x = x lsl Prec.v
	let to_int x = x asr Prec.v
	let of_nativeint x = Nativeint.to_int (Nativeint.shift_left x Prec.v)
	let to_nativeint x = Nativeint.shift_right (Nativeint.of_int x) Prec.v
	let of_int64 x = Int64.to_int (Int64.shift_left x Prec.v)
	let to_int64 x = Int64.shift_right (Int64.of_int x) Prec.v
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
	let of_float f = Nativeint.of_float (f *. (Nativeint.to_float one))
	let to_float s = (Nativeint.to_float s) /. (Nativeint.to_float one)
	let print ff s = Format.fprintf ff "%g" (to_float s)

	let mul a b =
		let m = Int64.shift_right (Int64.mul (Int64.of_nativeint a) (Int64.of_nativeint b)) Prec.v in
		Int64.to_nativeint m
	exception No_root
	let sqrt s = of_float (sqrt (to_float s))

	exception Not_invertible
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_nativeint a) Prec.v) (Int64.of_nativeint b) in
		Int64.to_nativeint m
	let inv s = div one s

	let rand = Random.nativeint

	exception Not_convertible
	let of_int x = Nativeint.shift_left (Nativeint.of_int x) Prec.v
	let to_int x = Nativeint.to_int (Nativeint.shift_right x Prec.v)
	let of_nativeint x = Nativeint.shift_left x Prec.v
	let to_nativeint x = Nativeint.shift_right x Prec.v
	let of_int64 x = Int64.to_nativeint (Int64.shift_left x Prec.v)
	let to_int64 x = Int64.shift_right (Int64.of_nativeint x) Prec.v
end

