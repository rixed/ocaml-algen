open Algen_intf

(* Some common dimensions for the lazy *)

module Dim1 : CONF_INT = struct let v = 1 end
module Dim2 : CONF_INT = struct let v = 2 end
module Dim3 : CONF_INT = struct let v = 3 end
module Dim4 : CONF_INT = struct let v = 4 end

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
end
module FloatRing : RING with type t = float = Ring (Core_FloatRing)

module Core_FloatField =
struct
	include FloatRing
	let inv x = 1./.x
	let sqrt = sqrt
	let half x = x *. 0.5
	let ceil = ceil
	let floor = floor
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

module FloatTrigo : TRIGO with type t = float =
struct
  include FloatField
  let pi = 4. *. atan 1.
  let sin = sin
  let cos = cos
  let tan = tan
  let asin = asin
  let acos = acos
  let atan = atan
  let sinh = sinh
  let cosh = cosh
  let tanh = tanh
end

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
end
module IntRing (Prec : CONF_INT) : RING with type t = int = Ring (Core_IntRing (Prec))

module IntField (Prec : CONF_INT) =
struct
	include IntRing (Prec)
	
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_int a) Prec.v) (Int64.of_int b) in
		Int64.to_int m
	let sqrt s = fixed_of_float Prec.v (sqrt (float_of_fixed Prec.v s))
	let trunc x = x land (lnot (one - 1))
	let ceil x =
		let t = trunc x in
		if t = x then x else t + one
	let floor x =
		let t = trunc x in
		if t = x then x else t
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

module MakeTrigo (F : FIELD) =
struct
  include F
  let pi = F.of_float (4. *. atan 1.)
  let sin x = F.of_float @@ sin @@ F.to_float x
  let cos x = F.of_float @@ cos @@ F.to_float x
  let tan x = F.of_float @@ tan @@ F.to_float x
  let asin x = F.of_float @@ asin @@ F.to_float x
  let acos x = F.of_float @@ acos @@ F.to_float x
  let atan x = F.of_float @@ atan @@ F.to_float x
  let sinh x = F.of_float @@ sinh @@ F.to_float x
  let cosh x = F.of_float @@ cosh @@ F.to_float x
  let tanh x = F.of_float @@ tanh @@ F.to_float x
end

module IntTrigo (Prec : CONF_INT) : TRIGO with type t = int =
  MakeTrigo (IntField (Prec))

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
end
module NatIntRing (Prec : CONF_INT) : RING with type t = nativeint = Ring (Core_NatIntRing (Prec))

module NatIntField (Prec : CONF_INT)
	: FIELD with type t = nativeint =
struct
	include NatIntRing (Prec)

	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_nativeint a) Prec.v) (Int64.of_nativeint b) in
		Int64.to_nativeint m
	let sqrt s = natfixed_of_float Prec.v (sqrt (float_of_natfixed Prec.v s))
	let trunc x = Nativeint.logand x (Nativeint.lognot (Nativeint.sub one 1n))
	let ceil x =
		let t = trunc x in
		if t = x then x else Nativeint.add t one
	let floor x =
		let t = trunc x in
		if t = x then x else t
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

module NatIntTrigo (Prec : CONF_INT) : TRIGO with type t = nativeint =
  MakeTrigo (NatIntField(Prec))
