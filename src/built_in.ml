(* built-in functions *)

open Printf
open Py_val

module H = Hashtbl

exception Built_in_error of string

(* helper functions - not in built-in scope *)
let rec print args =
  let print_in_literal pv =
    match pv with
      Str s -> printf "'%s'" s
    | _ -> print [pv]
  in
  let rec print' with_space args : unit =
  match args with
    []      -> ()
  | pv::pvs ->
      if with_space then
        printf " "
      else ();
      (match pv with
         Int i ->
           printf "%d" i
       | Float f ->
           let raw = sprintf "%.15g" f in
           let str = if String.contains raw '.' then raw else raw ^ ".0" in
           printf "%s" (Str.global_replace (Str.regexp "\\([^.]\\)0*$") "\\1" str)
       | Bool b ->
           if b then printf "True" else printf "False"
       | Str s ->
           printf "%s" s
       | Fun(name, _) ->
           printf "<Function `%s`>" name
       | Obj obj ->
           printf "<`%s` object>" obj.cls.name
       | Class cls ->
           printf "<Class `%s`>" cls.name
       | Type str ->
           printf "<Type `%s`>" str
       | List darr ->
           printf "[";
           let n = D.length darr in
           for i = 0 to n - 1 do
             print_in_literal (D.get darr i);
             if i <> n - 1 then printf ", " else ()
           done;
           printf "]"
       | Tuple arr ->
           printf "(";
           let n = Array.length arr in
           for i = 0 to n - 1 do
             print_in_literal arr.(i);
             if i <> n - 1 then printf ", " else ()
           done;
           printf ")"
       | Dict htbl ->
           let comma = ref "" in
           let f key value =
             printf "%s" !comma;
             comma := ", ";   (* comma is only empty in the first iteration *)
             print_in_literal key;
             printf ": ";
             print_in_literal value
           in
           printf "{"; H.iter f htbl; printf "}"
       | Seq seq ->
           (match seq () with
              Seq.Nil -> printf "<Sequence (empty)>"
            | Seq.Cons(pv, _) ->
                printf "<Sequence (";
                print [pv];
                printf ", ...)>")
       | None ->
           printf "None"
      );
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
    | List _ | Tuple _ | Dict _ | Seq _
    | Str _ | Fun _ | Obj _ | Class _ | Type _ | None ->
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
      | List _ | Tuple _ | Dict _ | Seq _
      | Float _ | Str _ | Fun _ | Obj _ | Class _ | Type _ | None ->
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
    else Bool (as_bool (List.hd args))

(* chr() -- only supports ASCII, doesn't match Python 3 exactly *)
let chr_ascii args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: CHR()")
  else
    let num =
      try as_int (List.hd args)
      with Type_error ->
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
    try Float (as_float (List.hd args))
    with Type_error ->
      raise (Built_in_error "Failed typecast: FLOAT_CAST()")

(* hex() and oct() *)
let hex_oct is_hex args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: HEX()")
  else
    let num =
      try as_int (List.hd args)
      with Type_error ->
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
    try Int (as_int (List.hd args))
    with Type_error ->
      raise (Built_in_error "Failed typecast: INT_CAST()")

(* len() *)
let len args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: LEN()")
  else
    match List.hd args with
      Str s -> Int (String.length s)
    | List darr -> Int (D.length darr)
    | Tuple arr -> Int (Array.length arr)
    | Dict htbl -> Int (H.length htbl)
    | Seq seq ->
        (* might take forever, who knows *)
        let count = ref 0 in
        Seq.iter (fun _ -> incr count) seq;
        Int !count
    | Int _ | Float _ | Bool _ | Obj _ | Fun _
    | Class _ | Type _ | None ->
        raise (Built_in_error "Non-sequence type: LEN()")

(* ord() -- only supports ASCII, doesn't match Python 3 exactly *)
let ord args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: ORD()")
  else
    let str =
      match List.hd args with
        Str s -> s
      | Int _ | Float _ | Bool _ | Fun _
      | List _ | Tuple _ | Dict _ | Seq _
      | Obj _ | Class _ | Type _ | None ->
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

(* range() *)
let range args =
  let (start, bound, step) =
    match args with
      [Int i] -> (0, i, 1)
    | [Int i; Int j] -> (i, j, 1)
    | [Int i; Int j; Int k] ->
        if k = 0 then raise (Built_in_error "Step cannot be zero: RANGE()")
        else (i, j, k)
    | _ -> raise (Built_in_error "At most three arguments expected: RANGE()")
  in
  let compare = if bound < start then (>) else (<) in
  let make_thunk () =
    let next = ref start in
    let rec do_step () =
      let stepped = !next + step in
      if compare stepped bound then (
        next := stepped;
        (fun () -> Seq.Cons(Int stepped, do_step ()))
      )
      else
        (fun () -> Seq.Nil)
    in
    if (step > 0 && bound > start) || (step < 0 && bound < start) then
      (fun () -> Seq.Cons(Int start, do_step ()))
    else
      (fun () -> Seq.Nil)
  in
  Seq (make_thunk ())

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
            | List _ | Tuple _ | Dict _ | Seq _
            | Str _ | Fun _ | Obj _ | Class _ | Type _ | None ->
                raise (Built_in_error "Cannot round non-numeric type: ROUND()")
    in
    let d = match digits with
              Int i  -> i
            | Bool b -> if b then 1 else 0
            | List _ | Tuple _ | Dict _ | Seq _
            | Float _ | Str _ | Fun _ | Obj _ | Class _ | Type _ | None ->
                raise (Built_in_error "Precision must be integer: ROUND()")
    in
    (* round to nearest integer *)
    let round' x = floor (x +. 0.5) in
    let factor = 10.0 ** (float_of_int d) in
    Float ((round' (n *. factor)) /. (factor))

(* str() *)
let rec str args =
  let from_str pv =
    match pv with
      Str s -> s
    | _ -> raise Type_error
  in
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: STR()")
  else
    match List.hd args with
      Int i -> Str (string_of_int i)
    | Float f ->
        let raw = sprintf "%.15g" f in
        let str = if String.contains raw '.' then raw else raw ^ ".0" in
        Str (sprintf "%s" (Str.global_replace (Str.regexp "\\([^.]\\)0*$") "\\1" str))
    | Bool b -> Str (if b then "True" else "False")
    | Str s -> Str s
    | Fun(name, _) -> Str (sprintf "<Function `%s`>" name)
    | Obj obj -> Str (sprintf "<`%s` object>" obj.cls.name)
    | Class cls -> Str (sprintf "<Class `%s`>" cls.name)
    | Type str -> Str (sprintf "<Type `%s`>" str)
    | List darr ->
        let out_string = ref "[" in
        let n = D.length darr in
        for i = 0 to n - 1 do
          out_string := !out_string ^ from_str (str [D.get darr i]);
          if i <> n - 1 then
            out_string := !out_string ^ ", "
          else ()
        done;
        Str (!out_string ^ "]")
    | Tuple arr ->
        let out_string = ref "(" in
        let n = Array.length arr in
        for i = 0 to n - 1 do
          out_string := !out_string ^ from_str (str [arr.(i)]);
          if i <> n - 1 then
            out_string := !out_string ^ ", "
          else ()
        done;
        Str (!out_string ^ ")")
    | Dict htbl ->
        let out_string = ref "" in
        let comma = ref "" in
        let f key value =
          out_string := !out_string ^ !comma;
          comma := ", ";   (* comma is only empty in the first iteration *)
          out_string := !out_string ^ from_str (str [key]);
          out_string := !out_string ^ ": ";
          out_string := !out_string ^ from_str (str [value])
        in
        H.iter f htbl;
        Str ("{" ^ !out_string ^ "}")
    | Seq seq ->
        (match seq () with
           Seq.Nil -> Str "<Sequence (empty)>"
         | Seq.Cons(pv, _) ->
             (match str [pv] with
                Str s ->
                  Str ("<Sequence (" ^ s ^ ", ...)>")
              | _ -> raise Type_error))
    | None ->
        Str "None"

(* type() *)
let get_type args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: TYPE()")
  else
    match List.hd args with
      Int _   -> Type "int"
    | Float _ -> Type "float"
    | Str _   -> Type "str"
    | Bool _  -> Type "bool"
    | Fun _   -> Type "function"
    | Obj o   -> Type o.cls.name
    | Class _ -> Type "type"
    | Type _  -> Type "type"
    | List _  -> Type "list"
    | Tuple _ -> Type "tuple"
    | Dict _  -> Type "dict"
    | Seq _   -> Type "sequence"
    | None    -> Type "NoneType"

(* these functions rely on type() *)

(* issubclass() *)
let rec issubclass args =
  match args with
    [pv1; pv2] ->
      (match (pv1, pv2) with
         (Class c1, Class c2) ->
           if c1.name = c2.name then
             Bool true
           else
             (match c1.super with
                None ->
                  Bool false
              | Some sc ->
                  issubclass [Class sc; pv2])
       | _ ->
           raise (Built_in_error "Both arguments must be classes: ISSUBCLASS()"))
  | _ ->
      raise (Built_in_error "Exactly two arguments expected: ISSUBCLASS()")

(* isinstance() *)
let isinstance args =
  match args with
    [pv1; pv2] ->
      (match pv2 with
         Class c ->
           (match pv1 with
              Obj o ->
                if o.cls.name = c.name then
                  Bool true
                else
                  (match o.cls.super with
                     Some superclass -> issubclass [Class superclass; pv2]
                   | None -> Bool false)
            | _ -> Bool false)
       | Type t ->
           (match get_type [pv1] with
              Type str -> Bool (t = str)
            | _ -> Bool false)
       | _ ->
           raise (Built_in_error "Second argument must be a class or type: ISINSTANCE()")
      )
  | _ ->
      raise (Built_in_error "Exactly two arguments expected: ISINSTANCE()")

(* string methods *)

(* find() *)
let find args =
  match args with
    [pv1; pv2] ->
      (match (pv1, pv2) with
         (Str s1, Str s2) ->
           (Int (try Str.search_forward (Str.regexp_string s2) s1 0
                 with Not_found -> -1))
       | _ -> raise (Built_in_error "Method expects a string: FIND()"))
  | _ ->
      raise (Built_in_error "Method expects one argument: FIND()")

let is_lower_alpha c = Char.code c >= 97 && Char.code c <= 122
let is_upper_alpha c = Char.code c >= 65 && Char.code c <= 90
let is_numeric c = Char.code c >= 48 && Char.code c <= 57

(* isalpha() *)
let is_alpha args =
  match args with
    [(Str s1)] ->
      let all_alpha = ref true in
      String.iter (fun c -> if is_lower_alpha (Char.lowercase_ascii c) then ()
                            else all_alpha := false) s1;
      Bool !all_alpha
  | _ -> raise (Built_in_error "Method expects exactly one string: ISALPHA()")

(* islower() *)
let is_lower args =
  match args with
    [(Str s1)] ->
      let all_lower = ref true in
      String.iter (fun c -> if is_lower_alpha c then () else all_lower := false) s1;
      Bool !all_lower
  | _ -> raise (Built_in_error "Method expects exactly one string: ISLOWER()")

(* isdigit() *)
let isdigit args =
  match args with
    [(Str s1)] ->
      let all_num = ref true in
      String.iter (fun c -> if is_numeric c then () else all_num := false) s1;
      Bool !all_num
  | _ -> raise (Built_in_error "Method expects exactly one string: ISNUMERIC()")

(* isupper() *)
let is_upper args =
  match args with
    [(Str s1)] ->
      let all_upper = ref true in
      String.iter (fun c -> if is_upper_alpha c then () else all_upper := false) s1;
      Bool !all_upper
  | _ -> raise (Built_in_error "Method expects exactly one string: ISUPPER()")

(* built-in scope *)
let table : (string, Py_val.t) Hashtbl.t =
  let tbl = H.create 33 in
  H.add tbl "abs" (Fun ("abs", abs));
  H.add tbl "bin" (Fun ("bin", bin));
  H.add tbl "bool" (Fun ("bool", bool_cast));
  H.add tbl "chr" (Fun ("chr", chr_ascii));
  H.add tbl "float" (Fun ("float", float_cast));
  H.add tbl "hex" (Fun ("hex", (hex_oct true)));
  H.add tbl "input" (Fun ("input", input));
  H.add tbl "isinstance" (Fun ("isinstance", isinstance));
  H.add tbl "issubclass" (Fun ("issubclass", issubclass));
  H.add tbl "int" (Fun ("int", int_cast));
  H.add tbl "len" (Fun ("len", len));
  H.add tbl "oct" (Fun ("oct", (hex_oct false)));
  H.add tbl "ord" (Fun ("ord", ord));
  H.add tbl "print" (Fun ("print", print_ln));
  H.add tbl "range" (Fun ("range", range));
  H.add tbl "round" (Fun ("round", round));
  H.add tbl "str" (Fun ("str", str));
  H.add tbl "type" (Fun ("type", get_type));
  tbl

let str_methods : (string, Py_val.t) Hashtbl.t =
  let tbl = H.create 10 in
  H.add tbl "find" (Fun ("find", find));
  H.add tbl "isalpha" (Fun ("isalpha", is_alpha));
  H.add tbl "islower" (Fun ("islower", is_lower));
  H.add tbl "isdigit" (Fun ("isdigit", isdigit));
  H.add tbl "isupper" (Fun ("isupper", is_upper));
  tbl
