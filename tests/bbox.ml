(* Test BBoxes. *)

open Algen_intf
open Algen_impl

module K = FloatField
module V = Vector (K) (struct let v = 2 end)
module Ve = ExtendedVector (V)
open Ve.Bbox

let v = Ve.make_unit 0

let check =
	assert (is_empty empty) ;
	assert (is_empty (union empty empty)) ;
	assert (not (is_empty (make v))) ;
	assert (not (is_empty (add empty v)))

