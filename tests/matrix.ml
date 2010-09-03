(* Test Matrix operations. *)

open Algen_intf
open Algen_impl

module Dim2 = struct let v = 2 end
module Dim3 = struct let v = 3 end

module K = FloatField
module V = Vector (K) (Dim2)
module Ve = ExtendedVector (V)
module M1e = ExtendedMatrix (Matrix (K) (Dim3) (Dim2))	(* 3 columns and 2 rows, or 3 vectors of 2 scalars *)
module M2e = ExtendedMatrix (Matrix (K) (Dim2) (Dim3))	(* the other way around *)
module MO = MatrixOps (M1e) (M2e)

let check_mul =
	assert (M1e.mul_scal 2.
		[| [| 1. ; 2. |] ; [| 3. ; 4. |] ; [| 5. ; 6. |] |] =
		[| [| 2. ; 4. |] ; [| 6. ; 8. |] ; [| 10. ; 12. |] |]) ;
	assert (M1e.mul_vec
		[| [| 1. ; 2. |] ; [| 3. ; 4. |] ; [| 5. ; 6. |] |]
	   	[| 1. ; 2. ; 3. |] = [| 22. ; 28. |]) ;
	assert (MO.mul
		[| [| 1. ; 2. |] ; [| 3. ; 4. |] ; [| 5. ; 6. |] |]
		[| [| 1. ; 2. ; 3. |] ; [| 4. ; 5. ; 6. |] |] =
		[| [| 22. ; 28. |] ; [| 49. ; 64. |] |]) ;
