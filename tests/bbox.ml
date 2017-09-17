(* Test BBoxes. *)
open Algen_impl

module K = FloatField
module V = Algen_vector.Make (K) (struct let v = 2 end)
open V.Bbox

let v = V.make_unit 0

let check =
	assert (is_empty empty) ;
	assert (is_empty (union empty empty)) ;
	assert (not (is_empty (make v))) ;
	assert (not (is_empty (add empty v)))

