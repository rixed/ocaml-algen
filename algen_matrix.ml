open Algen_intf

module Make
	(K : FIELD)
	(DimCol : CONF_INT)
	(DimRow : CONF_INT) :
	MATRIX with module K = K and module DimCol = DimCol and module DimRow = DimRow and type t = K.t array array =
struct
	module K = K
	module DimCol = DimCol
	module DimRow = DimRow
	module V = Algen_vector.Make (K) (DimRow)

	module Matrix_Core_Group =
	struct
		type t = V.t array
		let zero = Array.make DimCol.v V.zero
		let add a b = Array.init DimCol.v (fun i -> V.add a.(i) b.(i))
		let neg a = Array.init DimCol.v (fun i -> V.neg a.(i))
		let compare a b = array_compare V.compare a b
		let print fmt a =
			Array.iteri (fun i v ->
				if i = 0 then Format.fprintf fmt "@[{MAT:@ " ;
				V.print fmt v ;
				if i < DimCol.v - 1 then Format.fprintf fmt ",@ " else Format.fprintf fmt "}@]"
			) a
	end

	include Group (Matrix_Core_Group)

	let transpose m =
		Array.init DimCol.v (fun vi ->
			Array.init V.Dim.v (fun si -> m.(si).(vi)))
	
	let id = Array.init DimCol.v V.make_unit

	let make_unit vi si =
		Array.init DimCol.v (fun i ->
			if i = vi then V.make_unit si else V.zero)
	
	let init f =
		Array.init DimCol.v (fun c ->
			Array.init DimRow.v (fun r -> f c r))

	let mul_scal s a = Array.init DimCol.v (fun i -> V.mul s a.(i))

	let mul_vec m v =
		Array.init V.Dim.v (fun i ->
			let rec aux res j =
				if j < 0 then res else
				let c = K.mul m.(j).(i) v.(j) in
				aux (K.add res c) (j-1) in
			aux K.zero (DimCol.v-1))

	let mul_mat m1 m2 =
		Array.init (Array.length m2) (fun i ->
			mul_vec m1 m2.(i))

	(* mul_vec m v0 = v -> inv_mul m v = v0 *)
	let inv_mul mo vo =
		let copy_mat m = init (fun c r -> m.(c).(r)) in
		let m = copy_mat mo in
		let v = Array.copy vo in
		let swap_rows r1 r2 =
			for c = 0 to DimCol.v - 1 do
				let tmp = m.(c).(r1) in
				m.(c).(r1) <- m.(c).(r2) ;
				m.(c).(r2) <- tmp
			done ;
			let tmp = v.(r1) in
			v.(r1) <- v.(r2) ;
			v.(r2) <- tmp in
		let divide_row r c =
			let d = m.(c).(r) in
			m.(c).(r) <- K.one ;
			for c' = c + 1 to DimCol.v - 1 do
				m.(c').(r) <- K.div m.(c').(r) d
			done ;
			v.(r) <- K.div v.(r) d in
		let substract_row r_dst r c =
			let s = m.(c).(r_dst) in
			m.(c).(r_dst) <- K.zero ;
			for c' = c to DimCol.v - 1 do
				m.(c').(r_dst) <- K.sub m.(c').(r_dst) (K.mul s m.(c').(r))
			done ;
			v.(r_dst) <- K.sub v.(r_dst) (K.mul s v.(r)) in
		let c = ref 0
		and r = ref 0 in
		while !c < DimCol.v && !r < DimRow.v do
			(* Find pivot in column c, starting in row r *)
			let maxr = ref !r in
			for k = !r + 1 to DimRow.v - 1 do
				if K.abs m.(!c).(k) > K.abs m.(!c).(!maxr) then
					maxr := k
			done ;
			if 0 != K.compare m.(!c).(!maxr) K.zero then (
				swap_rows !r !maxr ;
				divide_row !r !c ;
				for k = !r + 1 to DimRow.v - 1 do
					substract_row k !r !c
				done ;
				incr r
			) ;
			incr c
		done ;
		(* Now solve *)
		let res = Array.make DimCol.v K.zero in
		for r = DimRow.v - 1 downto 0 do
			res.(r) <- v.(r) ;
			for c = r + 1 to DimCol.v - 1 do
				res.(r) <- K.sub res.(r) (K.mul m.(c).(r) res.(c))
			done
		done ;
		res
end

