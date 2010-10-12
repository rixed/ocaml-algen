open Algen_intf

module Make
	(K : FIELD)
	(Dim : CONF_INT) :
	VECTOR with module K = K and module Dim = Dim and type t = K.t array =
struct
	module K = K
	module Dim = Dim

	module Vector_Core_Group =
	struct
		type t = K.t array (* of size dim *)
		let zero = Array.make Dim.v K.zero
		let add a b = Array.init Dim.v (fun i -> K.add a.(i) b.(i))
		let neg a = Array.init Dim.v (fun i -> K.neg a.(i))
		let compare a b = array_compare K.compare a b
		let print fmt a =
			Array.iteri (fun i s ->
				if i = 0 then Format.fprintf fmt "@[{VEC: " ;
				K.print fmt s ;
				if i < Dim.v - 1 then Format.fprintf fmt ",@ " else Format.fprintf fmt "}@]")
				a
	end

	include Group (Vector_Core_Group)

	let mul s a = Array.init Dim.v (fun i -> K.mul s a.(i))

	let make_unit d = Array.init Dim.v (fun i -> if i = d then K.one else K.zero)

	let sub a b = Array.init Dim.v (fun i -> K.sub a.(i) b.(i))

	let half a = Array.init Dim.v (fun i -> K.half a.(i))

	let scalar_product a b =
		let res = ref K.zero in
		for i = 0 to (Dim.v-1) do res := K.add !res (K.mul a.(i) b.(i)) done ;
		!res

	let norm2 a = scalar_product a a

	let norm a = K.sqrt (norm2 a)

	let vect_product a b =
		let dual = Array.init Dim.v (fun i ->
			let j = if i + 1 >= Dim.v then 0 else i + 1  in
			K.sub (K.mul a.(i) b.(j)) (K.mul a.(j) b.(i))) in
		Array.init Dim.v (fun i ->
			let res = ref K.zero in
			for j = 1 to (Dim.v-1) do
				let j' = if i + j < Dim.v then i + j else i + j - Dim.v in
				res := K.add !res dual.(j')
			done;
			!res)

	let opposite a =
		Array.init Dim.v (fun i -> K.neg a.(i))

	let normalize a =
		let s = K.inv (norm a) in
		mul s a

	(* BBOX *)

	module Bbox : BBOX with type vector = t =
	struct
		type vector = t
		type t = Empty | Box of vector * vector

		let empty = Empty
		let is_empty b = b = Empty

		let make v = Box (v, v)

		let union b1 b2 = match b1, b2 with
			| Empty, b -> b
			| b, Empty -> b
			| Box (am, aM), Box (bm, bM) ->
				Box (Array.init Dim.v (fun i -> K.min am.(i) bm.(i)),
				     Array.init Dim.v (fun i -> K.max aM.(i) bM.(i)))

		let add b v = union b (make v)

		let print fmt = function
			| Empty ->
				Format.fprintf fmt "@[{BBOX:empty}@]"
			| Box (am, aM) ->
				Format.fprintf fmt "@[{BBOX:@ %a ->@ %a }@]" print am print aM
	end
end
