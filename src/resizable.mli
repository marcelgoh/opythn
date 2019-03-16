(* Resizable array used to represent OPythn lists *)

type 'a t

exception Index_error of int * string
exception Value_error of string

(* number of elements in resizable array *)
val size : 'a t -> int

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

(* remove element at specified index *)
val remove : 'a t -> int -> unit

(* find element e and remove it from the list *)
val find_remove : 'a t -> 'a -> unit

(* append single element at the end *)
val append : 'a t -> 'a -> unit

(* extend first list by appending second list at the end *)
val extend : 'a t -> 'a t -> unit
