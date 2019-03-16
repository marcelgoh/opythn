(* Resizable array used to represent OPythn lists *)

type 'a t = {
  mutable arr : 'a array;
  mutable size : int;
}

exception Index_error of int * string
exception Value_error of string

(* number of elements in resizable array *)
let size r = r.size

(* capacity of the current underlying array *)
let capacity r = Array.length r.arr

(* makes capacity bigger *)
let enlarge_cap c =
  let double = 2 * c in
  if double > 10 then double else 10

(* get positive version of index *)
let pos_index r idx = if idx < 0 then idx + r.size else idx

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
    let i = pos_index r idx in
    Array.get r.arr i

(* set the element at specified index to e, supports negative indices *)
let set r idx e =
  if idx < (- r.size) || idx >= r.size then
    raise (Index_error ((- r.size), "set"))
  else
    let i = pos_index r idx in
    Array.set r.arr i e

(* grow underlying array of r to new length l, filling dummy slots with x *)
let grow r x l =
  let newarr = Array.make l x in
  for i = 0 to r.size - 1 do
    Array.unsafe_set newarr i (Array.unsafe_get r.arr i)
  done;
  r.arr <- newarr

(* halve the length of underlying array and copies everything over *)
let shrink r =
  (* r.size must not be 0 and r.size must be greater than ((capacity r) / 2) *)
  let newarr = Array.make ((capacity r) / 2) (Array.unsafe_get r.arr 0) in
  for i = 0 to r.size - 1 do
    Array.unsafe_set newarr i (Array.unsafe_get r.arr i)
  done;
  r.arr <- newarr

(* insert the element e at specified index, shifting everything else down *)
let insert r idx e =
  if idx < (- r.size) || idx >= r.size then
    raise (Index_error ((- r.size), "insert"))
  else
    let pos_idx = pos_index r idx in
    if r.size == capacity r then
      let newcap = enlarge_cap r.size in
      grow r e newcap; (* uses e as a dummy element to grow list *)
    (* loop through elements after index, shifting them one to the right *)
    let i = ref (r.size - 1) in
    while !i >= pos_idx  do
      Array.set r.arr (!i + 1) (Array.get r.arr !i);
      i := !i - 1
    done;
    Array.set r.arr pos_idx e;
    r.size <- r.size + 1

(* remove elements from indices lo (inclusive) to hi (exclusive) *)
let remove_in_range r lo hi =
  let diff = hi - lo in
  let pos_lo = pos_index r lo in
  for i = pos_lo to r.size - diff - 1 do
    Array.set r.arr i (Array.get r.arr (i + diff))
  done;
  r.size <- r.size - diff;
  if r.size < ((capacity r) / 3) then
    shrink r

(* remove element at specified index *)
let remove r idx = remove_in_range r idx (idx + 1)

(* find element e and remove it from the list *)
let find_remove r e =
  let module M = struct exception Break of int end in
  try
    for i = 0 to r.size - 1 do
      if Array.get r.arr i = e then begin
        (* element found, so shift all elements one over from the right *)
        for j = i to r.size - 2 do
          Array.set r.arr j (Array.get r.arr (j + 1))
        done;
        r.size <- r.size - 1;
        if r.size < ((capacity r) / 3) then shrink r;
        ignore (raise (M.Break i))
      end
    done;
    raise (Value_error "find_remove")
  with M.Break i -> ()

(* append single element at the end *)
let append r e =
  if r.size = (capacity r) then begin
    let newcap = enlarge_cap r.size in
    grow r e newcap
  end;
  Array.set r.arr r.size e;
  r.size <- r.size + 1

(* extend first list by appending second list at the end *)
let extend l1 l2 =
  for i = 0 to l2.size - 1 do
    append l1 (get l2 i)
  done
