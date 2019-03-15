(* Resizable array used to represent OPythn lists *)

type 'a t

exception Index_error of int * string

(* capacity of the current underlying array *)
val capacity : 'a t -> int

(* creates a new empty resizable *)
val create_empty : unit -> 'a t

(* create a new resizable of length n, filled with x *)
val create_fill : int -> 'a -> 'a t

(* get the element at specified index, supports negative indices as in Python *)
val get : 'a t -> int -> 'a

(* set the element at specified index to e, supports negative indices *)
val set : 'a t -> int -> 'a -> unit
