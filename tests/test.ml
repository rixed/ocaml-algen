(* Test the shipped implementations. *)

open Algen_intf
open Algen_impl

module CheckGroup (G : GROUP) =
struct
	let () =
		assert (G.compare G.zero G.zero = 0) ;
		assert (G.add G.zero G.zero = G.zero) ;
		assert (G.neg G.zero = G.zero) ;
		assert (G.sub G.zero G.zero = G.zero) ;
		assert (G.adds [] = G.zero) ;
		assert (G.adds [ G.zero ] = G.zero) ;
		assert (G.min G.zero G.zero = G.zero) ;
		assert (G.max G.zero G.zero = G.zero) ;
		assert (G.abs G.zero = G.zero) ;
		assert (G.double G.zero = G.zero)
end

module CheckRing (R : RING) =
struct
	module ChkGrp = CheckGroup (R)
	module R = CheckedRing (R) (struct let v = true end)
	let () =
		assert (R.add R.one R.zero = R.one) ;
		assert (R.add R.zero R.one = R.one) ;
		assert (R.add R.one (R.neg R.one) = R.zero) ;
		assert (R.sub R.one R.one = R.zero) ;
		assert (R.abs R.one = R.one) ;
		assert (R.abs (R.neg R.one) = R.one) ;
		assert (R.mul R.one R.one = R.one) ;
		assert (R.mul R.zero R.one = R.zero) ;
		assert (R.mul R.one R.zero = R.zero) ;
		assert (R.muls [] = R.one) ;
		assert (R.square R.one = R.one) ;
		assert (R.exponent R.one 10 = R.one)
end

module CheckField (K : FIELD) =
struct
	module ChkRng = CheckRing (K)
	module K = CheckedField (K)
	let () =
		assert (K.inv K.one = K.one) ;
		assert (K.sqrt K.one = K.one) ;
		assert (K.ceil  K.one = K.one) ;
		assert (K.floor K.one = K.one) ;
		let a = K.half K.one in
		assert (K.floor a = K.zero) ;
		assert (K.ceil a = K.one) ;
		let b = K.neg a in
		assert (K.floor b = K.neg K.one) ;
		assert (K.ceil b = K.zero)
end

module M1 = CheckField (FloatField)
module M2 = CheckField (IntField (struct let v = 10 end))
module M3 = CheckField (IntField (struct let v = 1 end))
module M4 = CheckField (NatIntField (struct let v = 10 end))
module M5 = CheckField (NatIntField (struct let v = 1 end))

