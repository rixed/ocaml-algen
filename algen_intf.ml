module type CONF_BOOL = sig val v : bool end
module type CONF_INT  = sig val v : int  end

(* GROUPS *)

module type GROUP =
sig
	type t

	val zero    : t
	val add     : t -> t -> t
	val neg     : t -> t

	exception Not_comparable
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

module ExtendedGroup (G : GROUP) =
struct
	include G

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

(* unitary RINGS *)

module type RING =
sig
	include GROUP
	val one : t
	val mul : t -> t -> t

	exception No_root
	val sqrt : t -> t
end

module CheckedRing
	(R : RING)
	(Abelian : CONF_BOOL) =
struct
	include CheckedGroup (R) (struct let v = true end)
	(* CheckedGroup will only include from R the elements of a GROUP : *)
	let one = R.one
	exception No_root

	let mul a b =
		check_assoc_and_neutral R.mul a b one Abelian.v

	let sqrt a =
		let b = R.sqrt a in
		assert (eq a (R.mul b b)) ;
		assert (eq (R.sqrt one) one) ;
		b
end

module ExtendedRing (R : RING) =
struct
	include R

	let muls l =
		let rec aux res = function
			| [] -> res
			| x::l' -> aux (mul res x) l' in
		aux one l
	
	let square a = mul a a

	let exponent x n =
		let res = ref one in
		for i = 1 to n do
			res := mul !res x
		done ;
		!res

	module Ge = ExtendedGroup (R)

	let sub = Ge.sub
	let adds = Ge.adds
	let min = Ge.min
	let max = Ge.max
	let abs = Ge.abs
	let double = Ge.double
end

(* FIELD *)

module type FIELD =
sig
	include RING (* where all elements but zero can be inversed for mul *)

	exception Not_invertible
	val inv : t -> t
end

module CheckedField (F : FIELD) =
struct
	include CheckedRing (F) (struct let v = true end)
	(* CheckedRing will only include the RING elements of F : *)
	exception Not_invertible

	let inv a = check_inversion F.inv mul a one
end

module ExtendedField (K : FIELD) =
struct
	include K

	let div a b = mul a (inv b)

	let half a =
		let semi_one = inv (add one one) in
		mul semi_one a
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

	exception Not_comparable
	let compare a b = array_compare K.compare a b

	let print fmt a =
		Array.iteri (fun i s ->
			if i = 0 then Format.fprintf fmt "@[<VEC:@ " ;
			K.print fmt s ;
			if i < Dim.v - 1 then Format.fprintf fmt ",@ " else Format.fprintf fmt ">@]")
			a
end

module ExtendedVector (V : VECTOR) =
struct
	include V
	module Ke = ExtendedField (K)
	module Ge = ExtendedGroup (K)

	let make_unit d = Array.init Dim.v (fun i -> if i = d then K.one else K.zero)

	let sub a b = Array.init Dim.v (fun i -> Ge.sub a.(i) b.(i))

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
			Ge.sub (K.mul a.(i) b.(j)) (K.mul a.(j) b.(i))) in
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
		type t = vector * vector

		let empty_bbox = zero, zero

		let bbox_make v = v, v

		let bbox_union (am, aM) (bm, bM) =
			Array.init Dim.v (fun i -> Ge.min am.(i) bm.(i)),
			Array.init Dim.v (fun i -> Ge.max aM.(i) bM.(i))

		let bbox_add a v = bbox_union a (bbox_make v)

		let print fmt (am, aM) =
			Format.printf fmt "@[<BBOX:@ %k ->@ %k >@]" (print am) am (print aM) aM
	end
end

(* Another interresting case : K^([1;n]x[1;p]) MATRIX *)

module type MATRIX =
sig
	module K : FIELD
	module DimCol : CONF_INT
	module DimRow : CONF_INT
	module V : VECTOR with module Dim = DimRow and module K = K

	include GROUP with type t = V.t array (* .(col).(row) *)
	val mul : K.t -> t -> t
end

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

	let mul s a = Array.init DimCol.v (fun i -> V.mul s a.(i))

	exception Not_comparable
	let compare a b = array_compare V.compare a b

	let print fmt a =
		Array.iter (V.print fmt) a
end

module ExtendedMatrix (M : MATRIX) =
struct
	include M
	module Ve = ExtendedVector (V)

	let transpose m =
		Array.init DimCol.v (fun vi ->
			Array.init V.Dim.v (fun si -> m.(si).(vi)))
	
	let id = Array.init DimCol.v Ve.make_unit

	let make_unit vi si =
		Array.init DimCol.v (fun i ->
			if i = vi then Ve.make_unit si else V.zero)
	
	(* trace, determinant, etc... *)
end

(* To be able to combine matrixes of different sizes *)
module MatrixOps
	(M1 : MATRIX)
	(M2 : MATRIX with module V.Dim = M1.DimCol and module K = M1.K) =
struct
	module K = M1.K

	let mul (m1:M1.t) (m2:M2.t) =
		Array.init M2.DimCol.v (fun vi ->
			Array.init M1.V.Dim.v (fun si ->
				let rec aux res k =
					if k < 0 then res else
					let c = K.mul m1.(k).(si) m2.(vi).(k) in
					aux (K.add res c) (k-1) in
				aux K.zero (M1.DimCol.v-1)))
end
