(* Test Matrix operations. *)

open Algen_intf
open Algen_impl

module Dim2 = struct let v = 2 end
module Dim3 = struct let v = 3 end

module K = FloatField
module V = Algen_vector.Make (K) (Dim2)
module M1 = Algen_matrix.Make (K) (Dim3) (Dim2)	(* 3 columns and 2 rows, or 3 vectors of 2 scalars *)
module M2 = Algen_matrix.Make (K) (Dim2) (Dim3)	(* the other way around *)
module M3 = Algen_matrix.Make (K) (Dim2) (Dim2)	(* a mere 2x2 matrix *)

let check_mul =
	assert (M1.mul_scal 2.
		[| [| 1. ; 2. |] ; [| 3. ; 4. |] ; [| 5. ; 6. |] |] =
		[| [| 2. ; 4. |] ; [| 6. ; 8. |] ; [| 10. ; 12. |] |]) ;
	assert (M1.mul_vec
		[| [| 1. ; 2. |] ; [| 3. ; 4. |] ; [| 5. ; 6. |] |]
	   	[| 1. ; 2. ; 3. |] = [| 22. ; 28. |]) ;
	assert (M1.mul_mat
		[| [| 1. ; 2. |] ; [| 3. ; 4. |] ; [| 5. ; 6. |] |]
		[| [| 1. ; 2. ; 3. |] ; [| 4. ; 5. ; 6. |] |] =
		[| [| 22. ; 28. |] ; [| 49. ; 64. |] |])

let check_inv_mul =
	assert (M3.inv_mul
		[| [| 2. ; 1. |] ; [| 1. ; 3. |] |]
		[| 10. ; 15. |] = [| 3. ; 4. |])
