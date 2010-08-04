(* Test the shipped implementations. *)

open Algen_intf
open Algen_impl

module CheckGroup (G : GROUP) =
struct
	module Ge = ExtendedGroup (CheckedGroup (G) (struct let v = true end))
	let () =
		assert (Ge.compare Ge.zero Ge.zero = 0) ;
		assert (Ge.add Ge.zero Ge.zero = Ge.zero) ;
		assert (Ge.neg Ge.zero = Ge.zero) ;
		assert (Ge.sub Ge.zero Ge.zero = Ge.zero) ;
		assert (Ge.adds [] = Ge.zero) ;
		assert (Ge.adds [ Ge.zero ] = Ge.zero) ;
		assert (Ge.min Ge.zero Ge.zero = Ge.zero) ;
		assert (Ge.max Ge.zero Ge.zero = Ge.zero) ;
		assert (Ge.abs Ge.zero = Ge.zero) ;
		assert (Ge.double Ge.zero = Ge.zero)
end

module CheckRing (R : RING) =
struct
	module ChkGrp = CheckGroup (R)
	module Re = ExtendedRing (CheckedRing (R) (struct let v = true end))
	let () =
		assert (Re.add Re.one Re.zero = Re.one) ;
		assert (Re.add Re.zero Re.one = Re.one) ;
		assert (Re.add Re.one (Re.neg Re.one) = Re.zero) ;
		assert (Re.sub Re.one Re.one = Re.zero) ;
		assert (Re.abs Re.one = Re.one) ;
		assert (Re.abs (Re.neg Re.one) = Re.one) ;
		assert (Re.mul Re.one Re.one = Re.one) ;
		assert (Re.mul Re.zero Re.one = Re.zero) ;
		assert (Re.mul Re.one Re.zero = Re.zero) ;
		assert (Re.sqrt Re.one = Re.one) ;
		assert (Re.muls [] = Re.one) ;
		assert (Re.square Re.one = Re.one) ;
		assert (Re.exponent Re.one 10 = Re.one)
end

module CheckField (K : FIELD) =
struct
	module ChkRng = CheckRing (K)
	module Ke = ExtendedField (CheckedField (K))
	let () =
		assert (Ke.inv Ke.one = Ke.one)
end

module M1 = CheckField (FloatField)
module M2 = CheckField (IntField (struct let v = 10 end))
module M3 = CheckField (IntField (struct let v = 0 end))


