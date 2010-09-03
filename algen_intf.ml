module type CONF_BOOL = sig val v : bool end
module type CONF_INT  = sig val v : int  end

exception Not_invertible
exception Not_comparable
exception Not_convertible
exception No_root

(* GROUPS *)

module type GROUP =
sig
	type t

	val zero    : t
	val add     : t -> t -> t
	val neg     : t -> t

	val compare : t -> t -> int

	val print   : Format.formatter -> t -> unit
end

module CheckedGroup
	(G : GROUP)
	(Abelian : CONF_BOOL) =
struct
	include G

	let eq a b = 0 = compare a b

	let check_assoc_and_neutral op a b neutral commut =
		let c = op a b in
		(* check associativity *)
		assert (eq (op c a) (op a (op b a))) ;
		assert (eq (op c b) (op a (op b b))) ;
		(* check neutral element *)
		assert (eq (op a neutral) a) ;
		assert (eq (op neutral a) a) ;
		assert (eq (op b neutral) b) ;
		assert (eq (op neutral b) b) ;
		assert (eq (op c neutral) c) ;
		assert (eq (op neutral c) c) ;
		(* check commutativity *)
		if commut then (
			assert (eq (op b a) c)
		) ;
		c

	let add a b =
		check_assoc_and_neutral add a b zero Abelian.v

	let check_inversion op op2 a neutral =
		let b = op a in
		assert (eq (op2 a b) neutral) ;
		assert (eq (op2 b a) neutral) ;
		b
	
	let neg a =
		check_inversion neg add a zero
	
	let last_compared = ref None	(* only if they differ we store them here *)
	let compare a b =
		let c = compare a b in
		assert (compare b a = -c) ;
		assert (compare (neg a) (neg b) = -c) ;
		if c <> 0 then (
			(match !last_compared with
				| None -> ()
				| Some (la, lb) ->
					let lc = compare la lb in
					assert (lc <> 0) ;
					if lc = c then assert (compare (add a la) (add b lb) == c)
					else assert (compare (add a lb) (add b la) == c)) ;
			last_compared := Some (a, b)) ;
		c
end

module GroupExtension (G : GROUP) =
struct
	open G
	let sub a b = add a (neg b)

	let adds l =
		let rec aux res = function
			| [] -> res
			| x::l' -> aux (add res x) l' in
		aux zero l

	let min a b = if compare a b = 1 then b else a

	let max a b = if compare a b = -1 then b else a

	let abs a = max a (neg a)

	let double a = add a a
end

module ExtendedGroup (G : GROUP) =
struct
	include G
	include GroupExtension (G)
end

(* unitary RINGS *)

module type RING =
sig
	include GROUP
	val one : t
	val mul : t -> t -> t

	val sqrt : t -> t
end

module CheckedRing
	(R : RING)
	(Abelian : CONF_BOOL) =
struct
	include CheckedGroup (R) (struct let v = true end)
	(* CheckedGroup will only include from R the elements of a GROUP : *)
	let one = R.one

	let mul a b =
		check_assoc_and_neutral R.mul a b one Abelian.v

	let sqrt a =
		let b = R.sqrt a in
		assert (eq a (R.mul b b)) ;
		assert (eq (R.sqrt one) one) ;
		b
end

module RingExtension (R : RING) =
struct
	open R
	include GroupExtension (R)

	let muls l = List.fold_left mul one l
	
	let square a = mul a a

	let exponent x n =
		let res = ref one in
		for i = 1 to n do
			res := mul !res x
		done ;
		!res

end

module ExtendedRing (R : RING) =
struct
	include R
	include RingExtension (R)
end

(* FIELD *)

module type FIELD =
sig
	include RING (* where all elements but zero can be inversed for mul *)

	val inv : t -> t

	val rand : t -> t
	(** [rand bound] returns a value between [zero] and bound. *)

	(* Many fields are similar to numbers and we want to be able to convert to/from then *)
	val of_int       : int -> t
	val to_int       : t -> int
	val of_float     : float -> t
	val to_float     : t -> float
	val of_nativeint : nativeint -> t
	val to_nativeint : t -> nativeint
	val of_int64     : int64 -> t
	val to_int64     : t -> int64
end

module CheckedField (K : FIELD) =
struct
	include CheckedRing (K) (struct let v = true end)
	(* CheckedRing will only include the RING elements of K : *)
	let of_int = K.of_int
	let to_int = K.to_int
	let of_float = K.of_float
	let to_float = K.to_float
	let of_nativeint = K.of_nativeint
	let to_nativeint = K.to_nativeint
	let of_int64 = K.of_int64
	let to_int64 = K.to_int64

	let inv a = check_inversion K.inv mul a one

	let rand bound =
		let r = K.rand bound in
		assert (compare r zero >= 0) ;
		assert (compare r bound < 0) ;
		r
end

module FieldExtension (K : FIELD) =
struct
	open K
	include RingExtension (K)
	include GroupExtension (K)

	let div a b = mul a (inv b)

	let half a =
		let semi_one = inv (add one one) in
		mul semi_one a
end

module ExtendedField (K : FIELD) =
struct
	include K
	include FieldExtension (K)
end

(* VECTOR SPACE *)

module type VECSPACE =
sig
	module K : FIELD
	include GROUP

	val mul : K.t -> t -> t
end

module CheckedVecSpace
	(V : VECSPACE) =
struct
	include CheckedGroup (V) (struct let v = true end)

	let mul s v =
		let v' = V.mul s v in
		assert (eq (V.mul s (add v v)) (add v' v')) ;
		assert (eq (V.mul (V.K.add s s) v) (add v' v')) ;
		assert (eq (V.mul (V.K.mul s s) v) (V.mul s (V.mul s v))) ;
		assert (eq (V.mul V.K.one v) v) ;
		assert (eq (V.mul V.K.zero v) zero) ;
		assert (eq (V.mul s zero) zero) ;
		assert (eq (V.mul (V.K.neg V.K.one) v) (neg v)) ;
		v'
end

(* INTERRESTING CASE : K^n *)

(* Compare arrays of _same_length_ *)
let array_compare elmt_compare a b =
	let dim = Array.length a in
	let rec aux i =
		if i >= dim then 0
		else let c = elmt_compare a.(i) b.(i) in
		if c <> 0 then c else aux (i+1) in
	aux 0

module type VECTOR =
sig
	module K : FIELD
	module Dim : CONF_INT

	include GROUP with type t = K.t array
	val mul : K.t -> t -> t
end

module Vector
	(K : FIELD)
	(Dim : CONF_INT) :
	VECTOR with module K = K and module Dim = Dim =
struct
	module K = K
	module Dim = Dim
	type t = K.t array (* of size dim *)
	let zero = Array.make Dim.v K.zero

	let add a b = Array.init Dim.v (fun i -> K.add a.(i) b.(i))

	let neg a = Array.init Dim.v (fun i -> K.neg a.(i))

	let mul s a = Array.init Dim.v (fun i -> K.mul s a.(i))

	let compare a b = array_compare K.compare a b

	let print fmt a =
		Array.iteri (fun i s ->
			if i = 0 then Format.fprintf fmt "@[{VEC:@ " ;
			K.print fmt s ;
			if i < Dim.v - 1 then Format.fprintf fmt ",@ " else Format.fprintf fmt "}@]")
			a
end

module VectorExtension (V : VECTOR) =
struct
	open V
	module Ke = ExtendedField (V.K)

	let make_unit d = Array.init Dim.v (fun i -> if i = d then K.one else K.zero)

	let sub a b = Array.init Dim.v (fun i -> Ke.sub a.(i) b.(i))

	let half a = Array.init Dim.v (fun i -> Ke.half a.(i))

	let scalar_product a b =
		let res = ref K.zero in
		for i = 0 to (Dim.v-1) do res := K.add !res (K.mul a.(i) b.(i)) done ;
		!res

	let norm2 a = scalar_product a a

	let norm a = K.sqrt (norm2 a)

	let vect_product a b =
		let dual = Array.init Dim.v (fun i ->
			let j = if i + 1 >= Dim.v then 0 else i + 1  in
			Ke.sub (K.mul a.(i) b.(j)) (K.mul a.(j) b.(i))) in
		Array.init Dim.v (fun i ->
			let res = ref K.zero in
			for j = 1 to (Dim.v-1) do
				let j' = if i + j < Dim.v then i + j else i + j - Dim.v in
				res := K.add !res dual.(j')
			done)

	let oposite a =
		Array.init Dim.v (fun i -> K.neg a.(i))

	let normalize a =
		let s = K.inv (norm a) in
		mul s a

	(* BBOX *)

	module Bbox =
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
				Box (Array.init Dim.v (fun i -> Ke.min am.(i) bm.(i)),
				     Array.init Dim.v (fun i -> Ke.max aM.(i) bM.(i)))

		let add b v = union b (make v)

		let print fmt = function
			| Empty ->
				Format.fprintf fmt "@[{BBOX:empty}@]"
			| Box (am, aM) ->
				Format.fprintf fmt "@[{BBOX:@ %a ->@ %a }@]" V.print am V.print aM
	end
end

module ExtendedVector (V : VECTOR) =
struct
	include V
	include VectorExtension (V)
end

(* Another interresting case : K^([1;n]x[1;p]) MATRIX *)

module type MATRIX =
sig
	module K : FIELD
	module DimCol : CONF_INT
	module DimRow : CONF_INT
	module V : VECTOR with module Dim = DimRow and module K = K

	include GROUP with type t = V.t array (* .(col).(row) *)
end

(* FIXME: move me out of intf file (into matrix.ml ? *)
module Matrix
	(K : FIELD)
	(DimCol : CONF_INT)
	(DimRow : CONF_INT) :
	MATRIX with module K = K and module DimCol = DimCol and module DimRow = DimRow =
struct
	module K = K
	module DimCol = DimCol
	module DimRow = DimRow
	module V = Vector (K) (DimRow)
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

module MatrixExtension (M : MATRIX) =
struct
	open M
	module Ve = ExtendedVector (V)
	module Ke = Ve.Ke

	let transpose m =
		Array.init DimCol.v (fun vi ->
			Array.init V.Dim.v (fun si -> m.(si).(vi)))
	
	let id = Array.init DimCol.v Ve.make_unit

	let make_unit vi si =
		Array.init DimCol.v (fun i ->
			if i = vi then Ve.make_unit si else V.zero)
	
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
			m.(c).(r) <- Ke.one ;
			for c' = c + 1 to DimCol.v - 1 do
				m.(c').(r) <- Ke.div m.(c').(r) d
			done ;
			v.(r) <- Ke.div v.(r) d in
		let substract_row r_dst r c =
			let s = m.(c).(r_dst) in
			m.(c).(r_dst) <- Ke.zero ;
			for c' = c to DimCol.v - 1 do
				m.(c').(r_dst) <- Ke.sub m.(c').(r_dst) (Ke.mul s m.(c').(r))
			done ;
			v.(r_dst) <- Ke.sub v.(r_dst) (Ke.mul s v.(r)) in
		let c = ref 0
		and r = ref 0 in
		while !c < DimCol.v && !r < DimRow.v do
			(* Find pivot in column c, starting in row r *)
			let maxr = ref !r in
			for k = !r + 1 to DimRow.v - 1 do
				if Ke.abs m.(!c).(k) > Ke.abs m.(!c).(!maxr) then
					maxr := k
			done ;
			if 0 != Ke.compare m.(!c).(!maxr) Ke.zero then (
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
		let res = Array.make DimCol.v Ke.zero in
		for r = DimRow.v - 1 downto 0 do
			res.(r) <- v.(r) ;
			for c = r + 1 to DimCol.v - 1 do
				res.(r) <- Ke.sub res.(r) (Ke.mul m.(c).(r) res.(c))
			done
		done ;
		res

	(* trace, determinant, etc... *)
end

module ExtendedMatrix (M : MATRIX) =
struct
	include M
	include MatrixExtension (M)
end

(* To be able to combine matrixes of different sizes *)
module MatrixOps
	(M1 : MATRIX)
	(M2 : MATRIX with module V.Dim = M1.DimCol and module K = M1.K) =
struct
	module K = M1.K
	module M1e = ExtendedMatrix (M1)
	module M2e = ExtendedMatrix (M2)

	let mul (m1:M1.t) (m2:M2.t) =
		Array.init M2.DimCol.v (fun i ->
			M1e.mul_vec m1 m2.(i))
end
