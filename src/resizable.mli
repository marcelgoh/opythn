(* Resizable array used to represent OPythn lists *)

type 'a t = {
  mutable arr : 'a array;
  mutable size : int;
}

exception Index_error of int * string
exception Value_error of string

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

(* insert the element e at specified index, shifting everything else down *)
val insert : 'a t -> int -> 'a -> unit

(* remove elements from indices lo (inclusive) to hi (exclusive) *)
val remove_in_range : 'a t -> int -> int -> unit

(* find element e and remove it from the list *)
val find_remove : 'a t -> 'a -> unit
