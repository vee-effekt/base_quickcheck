module type C_INTF = sig
  type 'a t
  val lift : 'a -> 'a t
  val i2f : Int.t t -> Float.t t
  val pair : 'a t -> 'b t -> ('a * 'b) t

end
module type GENERATOR = sig
  type 'a t
  module C : C_INTF
  type 'a c = 'a C.t


  val return : 'a c -> 'a t
  val bind : 'a t -> f:('a c -> 'b t) -> 'b t

  val choose : (float c * 'a t) list -> 'a t

  val int : lo:(int c) -> hi:(int c) -> int t
  val float : lo:(float c) -> hi:(float c) -> float t
  val bool : bool t

  val size : int t
  val with_size : 'a t -> size_c:(int c) -> 'a t

  val to_qc : 'a t -> ('a Base_quickcheck.Generator.t) c
  val to_fun : 'a t -> (size:int -> random:Splittable_random.State.t -> 'a) c


  type ('a,'r) recgen
  val recurse : ('a,'r) recgen -> 'r c -> 'a t
  val recursive : 'r c -> (('a,'r) recgen -> 'r c -> 'a t) -> 'a t
end