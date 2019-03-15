(* OPythn main front-end *)

open Resizable

let print_ints r =
  for i = 0 to r.size - 1 do
    Printf.printf "%d " (get r i)
  done;
  Printf.printf "\n"

let print_strs r =
  for i = 0 to r.size - 1 do
    Printf.printf "%s " (get r i)
  done;
  Printf.printf "\n"

let main () =
  let arr = create_fill 4 "four" in
  let empty = create_empty () in
  set arr 2 "three";
  set arr 1 "two";
  set arr 0 "one";
  insert arr 2 "sike";
  print_strs arr;
  Printf.printf "%d %d\n" (capacity arr) arr.size;
(*   remove_in_range arr 0 5; *)
  find_remove arr "sike";
  print_strs arr;
  Printf.printf "%d %d\n" (capacity arr) arr.size;
  let narr = create_fill 4 4 in
  find_remove narr 4




let _ = main ()
