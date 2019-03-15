(* Resizable array used to represent OPythn lists *)

type 'a t = {
  mutable arr : 'a array;
  mutable size : int;
}

exception Index_error of int * string

(* capacity of the current underlying array *)
let capacity r = Array.length r.arr

(* creates a new empty resizable *)
let create_empty () = {
  arr = [||];
  size = 0
}

(* create a new resizable of length n, filled with x *)
let create_fill n x =
  let a = Array.make n x in
  { arr = a; size = n }

(* get the element at specified index, supports negative indices as in Python *)
let get r idx =
  if idx < ~-(r.size) || idx >= r.size then
    raise (Index_error (idx, "get"))
  else
    let i = if idx < 0 then idx + r.size else idx in
    Array.get r.arr i

(* set the element at specified index to e, supports negative indices *)
let set r idx e =
  if idx < (- r.size) || idx >= r.size then
    raise (Index_error ((- r.size), "set"))
  else
    let i = if idx < 0 then idx + r.size else idx in
    Array.set r.arr i e

(* grow underlying array of r to new length l, filling dummy slots with x *)
let grow r x l =
  let newarr = Array.make l x in
  for i = 0 to r.size - 1 do
    Array.unsafe_set newarr i (Array.unsafe_get r.arr i)
  done;
  r.arr <- newarr


