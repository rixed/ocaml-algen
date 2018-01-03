module type CONF_BOOL = sig val v : bool end
module type CONF_INT  = sig val v : int  end

exception Not_invertible
exception Not_comparable
exception Not_convertible
exception No_root

(* GROUPS *)

module type CORE_GROUP =
sig
  type t

  val zero    : t
  val add     : t -> t -> t
  val neg     : t -> t
  val compare : t -> t -> int
  val print   : Format.formatter -> t -> unit
  val rand    : t -> t
  (** [rand bound] returns a value between [zero] and bound. *)
end

module type GROUP =
sig
  include CORE_GROUP

  val sub    : t -> t -> t
  val adds   : t list -> t
  val min    : t -> t -> t
  val max    : t -> t -> t
  val abs    : t -> t
  val double : t -> t
  val eq     : t -> t -> bool
  val cond   : bool -> t -> t -> t

  module Infix : sig
    val ( +~ ) : t -> t -> t
    val ( -~ ) : t -> t -> t
    val ( ~-~ ) : t -> t
    val ( =~ ) : t -> t -> bool
    val ( <~ ) : t -> t -> bool
    val ( <=~ ) : t -> t -> bool
    val ( >~ ) : t -> t -> bool
    val ( >=~ ) : t -> t -> bool
  end
end

module Group (G : CORE_GROUP) :
  GROUP with type t = G.t =
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

  let eq a b = 0 = compare a b

  let cond c a b = if c then a else b

  module Infix = struct
    let ( +~ ) = add
    let ( -~ ) = sub
    let ( ~-~ ) = neg
    let ( =~ ) = eq
    let ( <~ ) a b = compare a b < 0
    let ( <=~ ) a b = compare a b <= 0
    let ( >~ ) a b = compare a b > 0
    let ( >=~ ) a b = compare a b >= 0
  end
end

module CheckedGroup
  (G : GROUP)
  (Abelian : CONF_BOOL) =
struct
  include G

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

  let last_compared = ref None  (* only if they differ we store them here *)
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

(* unitary RINGS *)

module type CORE_RING =
sig
  type t

  val one  : t
  val mul  : t -> t -> t
end

module type RING =
sig
  include CORE_RING

  val muls     : t list -> t
  val square   : t -> t
  val exponent : t -> int -> t
  val succ   : t -> t
  val pred   : t -> t

  module Infix : sig
    val ( +~ ) : t -> t -> t
    val ( -~ ) : t -> t -> t
    val ( ~-~ ) : t -> t
    val ( *~ ) : t -> t -> t
    val ( =~ ) : t -> t -> bool
    val ( <~ ) : t -> t -> bool
    val ( <=~ ) : t -> t -> bool
    val ( >~ ) : t -> t -> bool
    val ( >=~ ) : t -> t -> bool
  end

  include GROUP with type t := t and module Infix := Infix
end

module Ring (G : GROUP) (R : CORE_RING with type t = G.t) :
  RING with type t = R.t =
struct
  include R
  module Infix = struct
    include G.Infix
    let ( *~ ) = R.mul
  end
  include (G : GROUP with type t := t and module Infix := Infix)

  let muls l = List.fold_left mul one l

  let square a = mul a a

  let exponent x n =
    let res = ref one in
    for _i = 1 to n do
      res := mul !res x
    done ;
    !res

  let succ x = add x one
  let pred x = sub x one
end

module CheckedRing
  (R : RING)
  (Abelian : CONF_BOOL) =
struct
  module CG = CheckedGroup (R) (struct let v = true end)
  include R
  let add = CG.add
  let neg = CG.neg
  let compare = CG.compare
  let mul a b =
    CG.check_assoc_and_neutral R.mul a b one Abelian.v
end

(* FIELD *)

module type CORE_FIELD =
sig
  type t

  val inv          : t -> t
  val sqrt         : t -> t
  val half         : t -> t
  val floor        : t -> t
  val ceil         : t -> t

  (* Many fields are similar to numbers and we want to be able to convert to/from then *)
  val of_int       : int -> t
  val to_int       : t -> int
  val of_float     : float -> t
  val to_float     : t -> float
  val of_nativeint : nativeint -> t
  val to_nativeint : t -> nativeint
  val of_int64     : int64 -> t
  val to_int64     : t -> int64
  val of_string    : string -> t
  val to_string    : t -> string
end

module type FIELD =
sig
  include CORE_FIELD

  module Infix : sig
    val ( +~ ) : t -> t -> t
    val ( -~ ) : t -> t -> t
    val ( ~-~ ) : t -> t
    val ( *~ ) : t -> t -> t
    val ( /~ ) : t -> t -> t
    val ( =~ ) : t -> t -> bool
    val ( <~ ) : t -> t -> bool
    val ( <=~ ) : t -> t -> bool
    val ( >~ ) : t -> t -> bool
    val ( >=~ ) : t -> t -> bool
  end

  include RING with type t := t and module Infix := Infix
    (* where all elements but zero can be inversed for mul *)

  val div   : t -> t -> t
end

module Field (R : RING)
             (K : CORE_FIELD with type t = R.t) :
  FIELD with type t = R.t =
struct
  include K

  let div a b = R.mul a (inv b)

  module Infix = struct
    include R.Infix
    let ( /~ ) = div
  end

  include (R : RING with type t := t and module Infix := Infix)
end

module type TRIGO =
sig
  include FIELD
  val pi : t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan : t -> t
  val sinh : t -> t
  val cosh : t -> t
  val tanh : t -> t
end

module CheckedField (K : FIELD) =
struct
  include K
  module CR = CheckedRing (K) (struct let v = true end)
  let add = CR.CG.add
  let neg = CR.CG.neg
  let compare = CR.CG.compare
  let mul = CR.mul

  let inv a = CR.CG.check_inversion K.inv mul a one

  let sqrt a =
    let b = K.sqrt a in
    assert (eq a (K.mul b b)) ;
    assert (eq (K.sqrt one) one) ;
    b

  let half = K.half

  let floor a =
    let r = K.floor a in
    assert (compare r a <= 0) ;
    assert (compare r (sub a one) > 0) ;
    r

  let ceil a =
    let r = K.ceil a in
    assert (compare r a >= 0) ;
    assert (compare r (add a one) < 0) ;
    r

  let rand bound =
    let r = K.rand bound in
    assert (compare r zero >= 0) ;
    assert (compare r bound < 0) ;
    r
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

(* INTERESTING CASE : K^n *)

(* Compare arrays of _same_length_ *)
let array_compare elmt_compare a b =
  let dim = Array.length a in
  let rec aux i =
    if i >= dim then 0
    else let c = elmt_compare a.(i) b.(i) in
    if c <> 0 then c else aux (i+1) in
  aux 0

module type BBOX =
sig
  type vector
  type t = Empty | Box of vector * vector
  val empty     : t
  val is_empty  : t -> bool
  val make      : vector -> t
  val union     : t -> t -> t
  val intersect : t -> t -> bool
  val add       : t -> vector -> t
  val diagonal  : t -> vector
  val print     : Format.formatter -> t -> unit
  val translate : t -> vector -> t
end

module type VECTOR =
sig
(*  include VECSPACE with type t = K.t array *)
  module K    : FIELD
  module Dim  : CONF_INT
  module Bbox : BBOX with type vector = K.t array
  include GROUP with type t = K.t array

  val mul            : K.t -> t -> t
  val half           : t -> t
  val make_unit      : int -> t
  val one            : t
  val scalar_product : t -> t -> K.t
  val norm2          : t -> K.t
  val norm           : t -> K.t
  val vect_product   : t -> t -> t
  val opposite       : t -> t
  val normalize      : t -> t
  val copy           : t -> t

  (* Those operations modify their first operand ("i" is for inplace) *)

  val muli : K.t -> t -> unit
  val halfi : t -> unit
  val addi : t -> t -> unit
  val subi : t -> t -> unit
  val oppositei : t -> unit
  val normalizei : t -> unit

  (** [copyi a b] copies [b] into [a] - ie. destination first *)
  val copyi : t -> t -> unit
end

(* Another interesting case : K^([1;n]x[1;p]) MATRIX *)

module type CORE_MATRIX =
sig
  module K      : FIELD
  module DimCol : CONF_INT
  module DimRow : CONF_INT
  module V      : VECTOR with module Dim = DimRow and module K = K

  include GROUP with type t = K.t array array
end

module type MATRIX =
sig
  include CORE_MATRIX

  val transpose : t -> t
  val id        : t
  val make_unit : int -> int -> t
  val init      : (int -> int -> K.t) -> t
  val mul_scal  : K.t -> t -> t
  val mul_vec   : t -> V.t -> V.t
  val mul_mat   : t -> t -> t
  (** mul_mat works with compatibles matrices, the first one must be of
   * dimension DimCol, the other is not checked *)

  val inv_mul   : t -> V.t -> V.t
  (** [mul_vec m v0] = v  implies  [inv_mul m v] = v0 *)

  (* trace, determinant, etc... *)
end

