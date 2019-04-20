(* built-in functions *)

open Printf

let rec print (args : Py_val.t list) : Py_val.t =
  match args with
    []      -> printf "\n"; None
  | pv::pvs ->
      (match pv with
         Int i   -> printf "%d" i
       | Float f -> printf "%f" f
       | Bool b  -> if b then printf "True" else printf "False"
       | Str s   -> printf "%s" s
       | Fun f   -> printf "<function>"
       | None    -> printf "None");
      printf " ";
      print pvs
