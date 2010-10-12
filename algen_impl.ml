open Algen_intf

(* Some FIELDs to play with. *)

module Core_FloatGroup =
struct
	type t = float
	let zero = 0.
	let add = (+.)
	let neg x = -.x
	let compare = compare	(* From Pervasive *)
	let print ff s = Format.pp_print_float ff s
end
module FloatGroup : GROUP with type t = float = Group (Core_FloatGroup)

module Core_FloatRing =
struct
	include FloatGroup
	let one = 1.
	let mul = ( *.)
	let sqrt = sqrt
end
module FloatRing : RING with type t = float = Ring (Core_FloatRing)

module Core_FloatField =
struct
	include FloatRing
	let inv x = 1./.x
	let rand = Random.float
	let of_int = float_of_int
	let to_int = int_of_float
	let of_float x = x
	let to_float x = x
	let of_nativeint = Nativeint.to_float
	let to_nativeint = Nativeint.of_float
	let of_int64 = Int64.to_float
	let to_int64 = Int64.of_float
	let of_string = float_of_string
	let to_string = string_of_float
end

module FloatField : FIELD with type t = float = Field (Core_FloatField)


(* Fixed size precision based on int *)

let float_of_fixed prec f = (float_of_int f) /. (float_of_int (1 lsl prec))
let fixed_of_float prec f = int_of_float (f *. (float_of_int (1 lsl prec)))

module Core_IntGroup (Prec : CONF_INT) =
struct
	type t = int
	let zero = 0
	let add = (+)
	let neg s = -s
	let compare = compare
	let dec_mask = (1 lsl (Prec.v)) - 1
	let print ff s = Format.fprintf ff "%g" (float_of_fixed Prec.v s)
end
module IntGroup (Prec : CONF_INT) : GROUP with type t = int = Group (Core_IntGroup (Prec))

module Core_IntRing (Prec : CONF_INT) =
struct
	include IntGroup (Prec)
	let one = 1 lsl Prec.v
	let mul a b =
		let m = Int64.shift_right (Int64.mul (Int64.of_int a) (Int64.of_int b)) Prec.v in
		Int64.to_int m
	let sqrt s = fixed_of_float Prec.v (sqrt (float_of_fixed Prec.v s))
end
module IntRing (Prec : CONF_INT) : RING with type t = int = Ring (Core_IntRing (Prec))

module IntField (Prec : CONF_INT) =
struct
	include IntRing (Prec)
	
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_int a) Prec.v) (Int64.of_int b) in
		Int64.to_int m
	let half a = a asr 1
	let inv s = div one s
	let rand = Random.int
	let of_int x = x lsl Prec.v
	let to_int x = x asr Prec.v
	let of_float = fixed_of_float Prec.v
	let to_float = float_of_fixed Prec.v
	let of_nativeint x = Nativeint.to_int (Nativeint.shift_left x Prec.v)
	let to_nativeint x = Nativeint.shift_right (Nativeint.of_int x) Prec.v
	let of_int64 x = Int64.to_int (Int64.shift_left x Prec.v)
	let to_int64 x = Int64.shift_right (Int64.of_int x) Prec.v
	let of_string s = fixed_of_float Prec.v (float_of_string s)
	let to_string x = string_of_float (float_of_fixed Prec.v x)
end

(* Same as above, but using nativeints *)

let float_of_natfixed prec f = (Nativeint.to_float f) /. (Nativeint.to_float (Nativeint.shift_left 1n prec))
let natfixed_of_float prec f = Nativeint.of_float (f *. (Nativeint.to_float (Nativeint.shift_left 1n prec)))

module Core_NatIntGroup (Prec : CONF_INT) =
struct
	type t = nativeint
	let zero = 0n
	let add = Nativeint.add
	let neg = Nativeint.neg
	let compare = Nativeint.compare
	let print ff s = Format.fprintf ff "%g" (float_of_natfixed Prec.v s)
end
module NatIntGroup (Prec : CONF_INT) : GROUP with type t = nativeint = Group (Core_NatIntGroup (Prec))

module Core_NatIntRing (Prec : CONF_INT) =
struct
	include NatIntGroup (Prec)
	let one = Nativeint.shift_left 1n Prec.v
	let mul a b =
		let m = Int64.shift_right (Int64.mul (Int64.of_nativeint a) (Int64.of_nativeint b)) Prec.v in
		Int64.to_nativeint m
	let sqrt s = natfixed_of_float Prec.v (sqrt (float_of_natfixed Prec.v s))
end
module NatIntRing (Prec : CONF_INT) : RING with type t = nativeint = Ring (Core_NatIntRing (Prec))

module NatIntField (Prec : CONF_INT)
	: FIELD with type t = nativeint =
struct
	include NatIntRing (Prec)

	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_nativeint a) Prec.v) (Int64.of_nativeint b) in
		Int64.to_nativeint m
	let half a = Nativeint.shift_right a 1
	let inv s = div one s
	let rand = Random.nativeint
	let of_int x = Nativeint.shift_left (Nativeint.of_int x) Prec.v
	let to_int x = Nativeint.to_int (Nativeint.shift_right x Prec.v)
	let of_float = natfixed_of_float Prec.v
	let to_float = float_of_natfixed Prec.v
	let of_nativeint x = Nativeint.shift_left x Prec.v
	let to_nativeint x = Nativeint.shift_right x Prec.v
	let of_int64 x = Int64.to_nativeint (Int64.shift_left x Prec.v)
	let to_int64 x = Int64.shift_right (Int64.of_nativeint x) Prec.v
	let of_string s = natfixed_of_float Prec.v (float_of_string s)
	let to_string x = string_of_float (float_of_natfixed Prec.v x)
end

