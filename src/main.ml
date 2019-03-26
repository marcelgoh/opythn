(* OPythn main front-end *)

open Printf
open ExtLib

let main () =
  let arr = DynArray.make 10 in
  DynArray.add arr 16;
  printf "%d\n" (DynArray.get arr 0);
  printf "Hello world\n"




let _ = main ()
