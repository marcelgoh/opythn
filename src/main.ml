(* OPythn main front-end *)

open Printf
open Parser

let main () =
  let module D = DynArray in
  let arr = D.make 10 in
  D.add arr 16;
  printf "%d\n" (D.get arr 0);
  printf "Hello world\n"




let _ = main ()
