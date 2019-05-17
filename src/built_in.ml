(* built-in functions *)

open Printf
open Py_val

module H = Hashtbl

exception Built_in_error of string

(* helper functions - not in built-in scope *)
let print args =
  let rec print' with_space args =
  match args with
    []      -> None
  | pv::pvs ->
      if with_space then
        printf " "
      else ();
      (match pv with
         Int i   -> printf "%d" i
       | Float f -> let raw = sprintf "%.15g" f in
                    let str = if String.contains raw '.' then raw
                              else raw ^ ".0" in
           printf "%s" (Str.global_replace (Str.regexp "\\([^.]\\)0*$") "\\1" str)
       | Bool b  -> if b then printf "True" else printf "False"
       | Str s   -> printf "%s" s
       | Fun f   -> printf "<function>"
       | None    -> printf "None");
      print' true pvs in
  print' false args

(* all of the following functions are of type Py_val.t list -> Py_val.t *)

(* abs() *)
let abs args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: ABS()")
  else
    let num = List.hd args in
    match num with
      Int i -> Int (abs i)
    | Bool b -> if b then (Int 1) else (Int 0)
    | Float f -> Float (abs_float f)
    | Str _ | Fun _ | None ->
        raise (Built_in_error "Cannot operate on non-numeric type: ABS()")

(* bin() *)
let bin args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: BIN()")
  else
    let num =
      match List.hd args with
        Int i -> i
      | Bool b -> if b then 1 else 0
      | Float _ | Str _ | Fun _ | None ->
          raise (Built_in_error "Cannot operate on non integer type: BIN()")
    in
    let sign = if num < 0 then "-" else "" in
    let pos_num = if num < 0 then (-num) else num in
    let rec bin_of_int acc n =
      if n = 0 then acc
      else if n mod 2 = 0 then bin_of_int ("0" ^ acc) (n / 2)
           else bin_of_int ("1" ^ acc) (n / 2)
    in
    (Str (sign ^ "0b" ^ (bin_of_int "" pos_num)))

(* bool() *)
let bool_cast args =
  if List.length args = 0 then
    Bool false
  else
    if List.length args <> 1 then
      raise (Built_in_error "At most one argument expected: BOOL()")
    else
      match List.hd args with
        Int i -> Bool (i <> 0)
      | Bool b -> Bool b
      | Float f -> Bool (f <> 0.0)
      | Str s -> Bool (s <> "")
      | Fun f -> Bool true
      | None -> Bool false

(* chr() -- only supports ASCII, doesn't match Python 3 exactly *)
let chr_ascii args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: CHR()")
  else
    let num =
      match List.hd args with
        Int i -> i
      | Bool b -> if b then 1 else 0
      | Float _ | Str _ | Fun _ | None ->
          raise (Built_in_error "Cannot operate on non integer type: CHR()")
    in
    if num < 0 || num > 255 then
      raise (Built_in_error "Integer outside ASCII range: CHR()")
    else
      Str (String.make 1 (Char.chr num))

(* float() *)
let float_cast args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: FLOAT_CAST()")
  else
    match List.hd args with
      Int i   -> Float (float_of_int i)
    | Float f -> Float f
    | Str s   -> Float (float_of_string s)
    | Bool b  -> if b then Float 1.0 else Float 0.0
    | Fun _ | None ->
        raise (Built_in_error "Failed typecast: FLOAT_CAST()")

(* hex() and oct() *)
let hex_oct is_hex args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: HEX()")
  else
    let num =
      match List.hd args with
        Int i -> i
      | Bool b -> if b then 1 else 0
      | Float _ | Str _ | Fun _ | None ->
          raise (Built_in_error "Cannot operate on non integer type: HEX()")
    in
    let sign = if num < 0 then "-" else "" in
    let pos_num = if num < 0 then (-num) else num in
    if is_hex then
      Str (sign ^ "0x" ^ (sprintf "%x" pos_num))   (* hex() *)
    else
      Str (sign ^ "0o" ^ (sprintf "%o" pos_num))   (* oct() *)

(* input() *)
let input args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: INPUT()")
  else
    print args |> ignore;
    (Str (read_line ()))

(* int() *)
let int_cast args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: INT_CAST()")
  else
    match List.hd args with
      Int i   -> Int i
    | Float f -> Int (int_of_float f)
    | Str s   -> Int (int_of_string s)
    | Bool b  -> if b then Int 1 else Int 0
    | Fun _ | None ->
        raise (Built_in_error "Failed typecast: INT_CAST()")

(* ord() -- only supports ASCII, doesn't match Python 3 exactly *)
let ord args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: ORD()")
  else
    let str =
      match List.hd args with
        Str s -> s
      | Int _ | Float _ | Bool _ | Fun _ | None ->
          raise (Built_in_error "String expected: ORD()")
    in
    if String.length str <> 1 then
      raise (Built_in_error "String must be of length 1: ORD()")
    else
      (Int (Char.code str.[0]))

(* print() *)
let print_ln args =
  print args |> ignore;
  printf "\n";
  None

(* round() *)
let round args =
  if List.length args <> 2 then
    raise (Built_in_error "Exactly two arguments expected: ROUND()")
  else
    let (num, digits) = (List.hd args, List.hd @@ List.tl args) in
    let n = match num with
              Int i   -> float_of_int i
            | Bool b  -> if b then 1.0 else 0.0
            | Float f -> f
            | Str _ | Fun _ | None ->
                raise (Built_in_error "Cannot round non-numeric type: ROUND()")
    in
    let d = match digits with
              Int i  -> i
            | Bool b -> if b then 1 else 0
            | Float _ | Str _ | Fun _ | None ->
                raise (Built_in_error "Precision must be integer: ROUND()")
    in
    (* round to nearest integer *)
    let round' x = floor (x +. 0.5) in
    let factor = 10.0 ** (float_of_int d) in
    Float ((round' (n *. factor)) /. (factor))

(* built-in scope *)
let table : (string, Py_val.t) Hashtbl.t =
  let s = H.create 33 in
  H.add s "abs" (Fun abs);
  H.add s "bin" (Fun bin);
  H.add s "bool" (Fun bool_cast);
  H.add s "chr" (Fun chr_ascii);
  H.add s "float" (Fun float_cast);
  H.add s "hex" (Fun (hex_oct true));
  H.add s "input" (Fun input);
  H.add s "int" (Fun int_cast);
  H.add s "oct" (Fun (hex_oct false));
  H.add s "ord" (Fun ord);
  H.add s "print" (Fun print_ln);
  H.add s "round" (Fun round);
  s

