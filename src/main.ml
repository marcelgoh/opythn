(* OPythn main front-end *)

open Resizable

let main () =
  let arr = create_fill 4 "four" in
  let empty = create_empty () in
  Printf.printf "%d %d\n" (capacity arr) (capacity empty);
  Printf.printf "%s %s %s %s\n" (get arr 0) (get arr 1) (get arr 2) (get arr 3);
  set arr 2 "three";
  set arr 1 "two";
  set arr 0 "one";
  Printf.printf "%s %s %s %s\n" (get arr (-4)) (get arr (-3)) (get arr (-2)) (get arr (-1))

let _ = main ()
