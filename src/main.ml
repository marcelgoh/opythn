(* OPythn main front-end *)

open Resizable

let print_ints r =
  for i = 0 to (size r) - 1 do
    Printf.printf "%d " (get r i)
  done;
  Printf.printf "\n"

let print_strs r =
  for i = 0 to (size r) - 1 do
    Printf.printf "%s " (get r i)
  done;
  Printf.printf "\n"

let main () =
  let arr = create_fill 4 "four" in
  set arr 2 "three";
  set arr 1 "two";
  set arr 0 "one";
  insert arr 2 "sike";
  print_strs arr;
  Printf.printf "%d %d\n" (capacity arr) (size arr);
(*   remove_in_range arr 0 5; *)
  print_strs arr;
  Resizable.append arr "end";
  append arr "hi";
  print_strs arr;
  let sarr = create_fill 10 "uw" in
  extend arr sarr;
  print_strs arr;
  remove_in_range arr 3 16;
  remove arr 2;
  print_strs arr;
  Printf.printf "%d %d\n" (capacity arr) (size arr);
  let narr = create_fill 4 4 in
  find_remove narr 4;
  print_ints narr




let _ = main ()
