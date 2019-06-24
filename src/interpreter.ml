(* Bytecode interpreter *)

open Printf
open Py_val
module D = DynArray
module F = Float
module H = Hashtbl
module S = Stack

exception Runtime_error of string
exception Type_error
exception Bound_error of int

type scope = (string, Py_val.t) H.t

type env = {
  cls : Py_val.cls option;
  locals : scope list;
  globals : scope list;
}

let rec int_exp x n =
  if n = 0 then 1
  else
    let x' = int_exp x (n / 2) in
    if n mod 2 = 0 then x' * x'
    else x * x' * x'

(* environment functions *)
let rec lookup_scope_list s_list id =
  match s_list with
    [] ->
      raise (Runtime_error (Printf.sprintf "Variable `%s` not found: LOAD_GLOBAL" id))
  | s::ss ->
      (match H.find_opt s id with
         Some pv -> pv
       | None -> lookup_scope_list ss id)

let lookup_global envr id = lookup_scope_list envr.globals id

(* suppose f is a function that takes in a list of py_vals as argument
 * (create_method f obj) returns a function that takes in a list [a1; a2; ...]
 * and calls f [obj; a1; a2; ...]
 *)
let create_method f obj = (fun arglist -> f (obj :: arglist))

(* run code on virtual stack machine *)
let rec run (c : Bytecode.code) block_name (envr : env) : Py_val.t =
  (* stack machine *)
  let (stack : Py_val.t S.t) = S.create () in
  (* set to true if the next CALL_FUNCTION is a method call *)
  let s_push pv =
(*     printf "Pushed: %s\n" (str_of_py_val pv); *)
    S.push pv stack
  in
  let s_pop () : Py_val.t =
    let tos = S.pop stack in
(*     printf "Popped: %s\n" (str_of_py_val tos); *)
    tos
  in
  let store_name scope id =
    let tos = s_pop () in
    H.replace scope id tos
  in
  let module Loop =
    struct
      exception Exit of Py_val.t
    end
  in
  (* loop over instructions -- changed to a while-loop because OCaml couldn't perform TCO *)
  let loop () : Py_val.t =
    let program_counter = ref 0 in
    (* get positive index if i in list of length n *)
    let pos_idx idx n = if idx < 0 then idx + n else idx in
    let subscr_get_idx i n =
      (* converts to positive index *)
      let ret_val = if i < 0 then i + n else i in
      if ret_val < 0 || ret_val >= n then
        (* also checks bounds *)
        raise (Bound_error i)
      else ret_val
    in
    (* get the sub{string, list} length given two indices and list length n *)
    let sub_len i j n =
      let j' = pos_idx j n in
      if j' < 0 || j' > n then
        raise (Bound_error j')
      else ();
      if i > j' then 0 else j' - i
    in
    try (
      while !program_counter < D.length c do
        let next = ref (!program_counter + 1) in
        (match D.get c !program_counter with
           NOP -> ()
         | POP_TOP -> s_pop () |> ignore
         | UNARY_NEG ->
             let tos = s_pop () in
             (try if is_float tos then
                    s_push (Float (-. (as_float tos)))
                  else
                    s_push (Int (- (as_int tos)))
              with Type_error -> raise (Runtime_error "Wrong type: UNARY_NEG"))
         | UNARY_NOT ->
             s_push (Bool (not (as_bool (s_pop ()))))
         | UNARY_BW_COMP ->
             (try s_push (Int (lnot (as_int (s_pop ()))))
              with Type_error -> raise (Runtime_error "Wrong type: UNARY_BW_COMP"))
         | BINARY_ADD ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try
                (match (tos1, tos) with
                   (Str s, Str t) ->
                     s_push (Str (s ^ t))
                 | (List darr1, List darr2) ->
                     let c = D.copy darr2 in
                     D.append c darr1;
                     s_push (List darr1)
                 | (Tuple arr1, Tuple arr2) ->
                     s_push (Tuple (Array.append arr1 arr2))
                 | _              -> if is_float tos1 || is_float tos then
                                       s_push (Float ((as_float tos1) +. (as_float tos)))
                                     else
                                       s_push (Int ((as_int tos1) + (as_int tos))))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_ADD"))
         | BINARY_SUB ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try if is_float tos1 || is_float tos then
                    s_push (Float ((as_float tos1) -. (as_float tos)))
                  else
                    s_push (Int ((as_int tos1) - (as_int tos)))
               with Type_error -> raise (Runtime_error "Type mismatch: BINARY_SUB"))
         | BINARY_MULT ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try if is_float tos1 || is_float tos then
                    s_push (Float ((as_float tos1) *. (as_float tos)))
                  else
                    s_push (Int ((as_int tos1) * (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_MULT"))
         | BINARY_FP_DIV ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (match F.classify_float @@ as_float tos with
                FP_zero -> raise (Runtime_error "Division by zero: BINARY_FP_DIV")
              | _       ->
                (try s_push (Float ((as_float tos1) /. (as_float tos)))
                 with Type_error -> raise (Runtime_error "Type mismatch: BINARY_FP_DIV")))
         | BINARY_INT_DIV ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (match F.classify_float @@ as_float tos with
                FP_zero -> raise (Runtime_error "Division by zero: BINARY_INT_DIV")
              | _       ->
                (try if is_float tos1 || is_float tos then
                       s_push (Float (floor ((as_float tos1) /. (as_float tos))))
                     else
                       s_push (Int (int_of_float (floor ((as_float tos1) /. (as_float tos)))))
                 with Type_error -> raise (Runtime_error "Type mismatch: BINARY_INT_DIV")))
         | BINARY_MOD ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (match F.classify_float @@ as_float tos with
                FP_zero -> raise (Runtime_error "Division by zero: BINARY_MOD")
              | _       ->
                (try if is_float tos1 || is_float tos then
                       (* OPythn modulo differs from in OCaml *)
                       let quot = (floor ((as_float tos1) /. (as_float tos))) in
                       let ans = (as_float tos1) -. quot *. (as_float tos) in
                       s_push (Float ans)
                     else
                       let quot = int_of_float (floor ((as_float tos1) /. (as_float tos))) in
                       let ans = (as_int tos1) - quot * (as_int tos) in
                       s_push (Int ans)
                 with Type_error -> raise (Runtime_error "Type mismatch: BINARY_MOD")))
         | BINARY_EXP ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try if is_float tos1 || is_float tos then
                    let ans = ((as_float tos1) ** (as_float tos)) in
                    match F.classify_float ans with
                      F.FP_nan -> raise (Runtime_error "Got a NaN: BINARY_EXP")
                    | _        -> s_push (Float ans)
                  else
                    if as_int tos < 0 then
                      if as_int tos1 < 0 then
                        s_push (Float (-.(1.0 /. float_of_int ((int_exp (as_int tos1) (as_int tos))))))
                      else
                        s_push (Float (1.0 /. float_of_int ((int_exp (as_int tos1) (as_int tos)))))
                    else
                      if as_int tos1 < 0 then
                        s_push (Int (-(int_exp (as_int tos1) (as_int tos))))
                      else
                        s_push (Int (int_exp (as_int tos1) (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_EXP"))
         | BINARY_LSHIFT ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Int ((as_int tos1) lsl (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_LSHIFT"))
         | BINARY_RSHIFT ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Int ((as_int tos1) asr (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_RSHIFT"))
         | BINARY_BW_AND ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Int ((as_int tos1) land (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_BW_AND"))
         | BINARY_BW_OR ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Int ((as_int tos1) lor (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_BW_OR"))
         | BINARY_BW_XOR ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Int ((as_int tos1) lxor (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_BW_XOR"))
         | COMPARE_EQ ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (eq tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_EQ"))
         | COMPARE_NEQ ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (not (eq tos1 tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_NEQ"))
         | COMPARE_LT ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (lt tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_LT"))
         | COMPARE_GT ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (lt tos tos1)) (* switch order and call lt *)
              with type_error -> raise (Runtime_error "type mismatch: compare_gt"))
         | COMPARE_LEQ ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (not (lt tos tos1))) (* not gt *)
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_LEQ"))
         | COMPARE_GEQ ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (not (lt tos1 tos))) (* not lt *)
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_LEQ"))
         | COMPARE_IS ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (is tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_IS"))
         | COMPARE_IN ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (py_in tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_IN"))
         | COMPARE_NOT_IN ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (not (py_in tos1 tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_NOT_IN"))
         | COMPARE_IS_NOT ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (try s_push (Bool (not (is tos1 tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_IS_NOT"))
         | RETURN_VALUE -> raise (Loop.Exit (s_pop ())) (* return top of stack *)
         | STORE_LOCAL(n, id) ->
             (match List.nth_opt envr.locals n with
                None -> raise (Runtime_error "Tried to access non-existent scope: STORE_LOCAL")
              | Some scope -> store_name scope id)
         | STORE_GLOBAL id -> store_name (List.hd envr.globals) id
         | LOAD_CONST pv -> s_push pv
         | LOAD_LOCAL(n, id) ->
             (match List.nth_opt envr.locals n with
                None -> raise (Runtime_error "Tried to access non-existent scope: LOAD_LOCAL")
              | Some scope ->
                  (match H.find_opt scope id with
                     None -> raise (Runtime_error (sprintf "Variable `%s` not found: LOAD_LOCAL" id))
                   | Some pv -> s_push pv))
         | LOAD_GLOBAL id -> s_push @@ lookup_global envr id
         | JUMP t -> next := t
         | POP_JUMP_IF_FALSE t -> if as_bool (s_pop ()) then () else next := t
         | JUMP_IF_TRUE_OR_POP t ->
             if as_bool (S.top stack) then next := t
             else s_pop () |> ignore
         | JUMP_IF_FALSE_OR_POP t ->
             if not (as_bool (S.top stack)) then next := t
             else s_pop () |> ignore
         | CALL_FUNCTION argc ->
             let arglist = ref [] in
             for _ = 0 to argc - 1 do
               arglist := s_pop () :: !arglist
             done;
             let retval =
               match s_pop () with
                 Fun (_, f) -> f !arglist
               | Class c ->
                   let obj = Obj { cls = c; fields = H.create 10; } in
                   (* if class has __init__ method, call it *)
                   (match H.find_opt c.attrs "__init__" with
                      Some (Fun (_, f)) ->
                        f (obj :: !arglist) |> ignore;
                    | _ -> ()
                   );
                   obj
               | _     -> raise (Runtime_error "Tried to apply uncallable object: CALL_FUNCTION")
             in
             s_push retval
         | MAKE_FUNCTION (params, block) ->
             s_push (Fun (block.name,
                       (fun args ->
                          if List.length params <> List.length args then
                            raise (Runtime_error
                                     (sprintf "Wrong argument count for function `%s`: MAKE_FUNCTION"
                                              block.name))
                          else (
                            let new_locals = H.create 5 in
                            List.iter2 (fun param arg -> H.add new_locals param arg) params args;
                            (* run codeblock with new scope pushed onto locals list *)
                            run !(block.ptr) block.name { cls = None;
                                                          locals = new_locals :: envr.locals;
                                                          globals = envr.globals; }
                          ))))
         | MAKE_CLASS (num_supers, block) ->
             let tos_opt = if num_supers = 1 then Some (s_pop ()) else None in
             let super_opt : Py_val.cls option =
               match tos_opt with
                 None -> None
               | Some pval ->
                   (match pval with
                      Class c -> Some c
                    | _ -> raise (Runtime_error "Top of stack is not a class: MAKE_CLASS"))
             in
             let new_cls = { name = block.name;
                             super = super_opt;
                             attrs = H.create 10 }
             in
             let new_envr = { envr with cls = Some new_cls } in
             run !(block.ptr) block.name new_envr |> ignore;
             s_push (Class new_cls)
         | STORE_NAME id ->
             (* bind variable in innermost scope *)
             (match envr.cls with
                Some c ->
                  store_name c.attrs id
              | None ->
                  (match envr.locals with
                     (s :: _) -> store_name s id
                   | [] -> store_name (List.hd envr.globals) id))
         | LOAD_NAME id ->
             (* search all scopes *)
             let search_scopes id : Py_val.t =
               let rec iter slist : Py_val.t option =
                 (match slist with
                    [] -> None
                  | (s :: ss) ->
                      (match H.find_opt s id with
                         Some pval -> Some pval
                       | None -> iter ss))
               in
               (match iter envr.locals with
                  Some pval -> pval
                | None ->
                    (match iter envr.globals with
                       Some pval -> pval
                     | None -> raise (Runtime_error
                                       (sprintf "Variable `%s` not found: LOAD_NAME" id))))
             in
             let value_to_push =
               (match envr.cls with
                  Some c ->
                    (match H.find_opt c.attrs id with
                       Some pval -> pval
                     | None -> search_scopes id)
                    | None -> search_scopes id)
             in
             s_push value_to_push
         | STORE_ATTR id ->
             let tos = s_pop () in
             let tos1 = s_pop () in
             (match tos with
                Obj obj -> H.replace obj.fields id tos1
              | Class cls -> H.replace cls.attrs id tos1
              | _ -> raise (Runtime_error
                              (sprintf "Cannot set attribute `%s`: STORE_ATTR" id)))
         | LOAD_ATTR id ->
             let tos = s_pop () in
             (match tos with
                Obj obj ->
                  (match Py_val.get_field_opt obj id with
                     Some pval ->
                       (match pval with
                          Fun (_, f) ->
                            let cls_name = obj.cls.name in
                            let method_name = sprintf "%s.%s method of %s object" cls_name id cls_name in
                            s_push (Fun (method_name, (create_method f (Obj obj))))
                        | _ -> s_push pval)
                   | None ->
                       raise (Runtime_error
                                (sprintf "Object %s has no attribute `%s`: LOAD_ATTR"
                                         obj.cls.name id)))
              | Class cls ->
                  (match Py_val.get_attr_opt cls id with
                     Some pval -> s_push pval;
                   | None ->
                       raise (Runtime_error
                                (sprintf "Class has no attribute `%s`: LOAD_ATTR" id)))
              | Str s ->
                  (match H.find_opt Built_in.str_methods id with
                     Some (Fun (name, f)) ->
                       let method_name = sprintf "built-in method %s of str object" name in
                       s_push (Fun (method_name, (create_method f (Str s))))
                   | _ ->
                       raise (Runtime_error
                                (sprintf "Str object has no attribute `%s`: LOAD_ATTR" id)))
              | Dict htbl ->
                  (match H.find_opt Built_in.dict_methods id with
                     Some (Fun (name, f)) ->
                       let method_name = sprintf "built-in method %s of dict object" name in
                       s_push (Fun (method_name, (create_method f (Dict htbl))))
                   | _ ->
                       raise (Runtime_error
                                (sprintf "Dict object has no attribute `%s`: LOAD_ATTR" id)))
              | _ -> raise (Runtime_error
                              (sprintf "Object %s has no attribute `%s`: LOAD_ATTR"
                                       (str_of_py_val tos) id))
             );
         | BUILD_SEQ ->
             (try s_push (Seq (as_seq (s_pop ())))
              with Type_error ->
                raise (Runtime_error "Failed to build sequence: BUILD_SEQ"))
         | FOR_ITER program_counter ->
             let tos = s_pop () in
             (match tos with
                Seq s ->
                  (match s () with
                     Seq.Nil -> next := program_counter  (* end of sequence, jump forward *)
                   | Seq.Cons(pv, rest) ->
                       s_push (Seq rest);    (* push rest of sequence for next iteration *)
                       s_push pv)            (* this value will be assigned to iter var *)
              | _ ->
                  raise (Runtime_error "Failed to get next value: FOR_ITER"))
         | BUILD_TUPLE n ->
             let arr = Array.make n None in
             for i = 0 to n - 1 do
               (* populate array in reverse *)
               arr.(n - i - 1) <- s_pop ()
             done;
             s_push (Tuple arr)
         | BUILD_LIST n ->
             let darr = D.create () in
             for _ = 0 to n - 1 do
               (* build array in reverse *)
               D.insert darr 0 (s_pop ())
             done;
             s_push (List darr)
         | BUILD_DICT n ->
             let tbl = H.create ~random:false n in
             for _ = 0 to n - 1 do
               let v = s_pop () in
               let k = s_pop () in
               H.add tbl k v
             done;
             s_push (Dict tbl)
         | SUBSCR ->
             let tos = s_pop () in    (* TOS <- TOS1[TOS] *)
             let tos1 = s_pop () in
             (match tos1 with
                Str s ->
                  let idx = subscr_get_idx (as_int tos) (String.length s) in
                  s_push (Str (String.make 1 s.[idx]))
              | List darr ->
                  let idx = subscr_get_idx (as_int tos) (D.length darr) in
                  s_push (D.get darr idx)
              | Tuple arr ->
                  let idx = subscr_get_idx (as_int tos) (Array.length arr) in
                  s_push arr.(idx)
              | Dict htbl ->
                  (match H.find_opt htbl tos with
                     Some v -> s_push v
                   | None ->
                       raise (Runtime_error "Key not found: SUBSCR"))
              | _ ->
                  raise (Runtime_error "Object not subscriptable: SUBSCR"))
         | SLICESUB ->
             let get_i idx n =
               let i = pos_idx idx n in
               if i < 0 || i >= n then
                 raise (Bound_error i)
               else i
             in
             let tos = s_pop () in     (* TOS <- TOS2[TOS1:TOS] *)
             let tos1 = s_pop () in
             let tos2 = s_pop () in
             (match tos2 with
                Str s ->
                  let n = String.length s in
                  let i = get_i (as_int tos1) n in
                  s_push (Str (String.sub s i (sub_len i (as_int tos) n)))
              | List darr ->
                  let n = D.length darr in
                  let i = get_i (as_int tos1) n in
                  s_push (List (D.sub darr i (sub_len i (as_int tos) n)))
              | Tuple arr ->
                  let n = Array.length arr in
                  let i = get_i (as_int tos1) n in
                  s_push (Tuple (Array.sub arr i (sub_len i (as_int tos) n)))
              | _ -> raise (Runtime_error "Object not slice-subscriptable: SLICESUB"))
         | DELETE_LOCAL (n, id) ->
             (match List.nth_opt envr.locals n with
                None -> raise (Runtime_error "Tried to access non-existent scope: DELETE_LOCAL")
              | Some scope -> H.remove scope id)
         (* del operations will not give warning if nothing is being deleted *)
         | DELETE_GLOBAL id ->
             H.remove (List.hd envr.globals) id
         | DELETE_NAME id ->
             (* delete from innermost scope *)
             (match envr.cls with
                Some c ->
                  H.remove c.attrs id
              | None ->
                  (match envr.locals with
                     (s :: _) -> H.remove s id
                   | [] -> H.remove (List.hd envr.globals) id))
         | DELETE_ATTR id ->
             let tos = s_pop () in
             (match tos with
                Obj obj -> H.remove obj.fields id
              | Class cls -> H.remove cls.attrs id
              | _ -> raise (Runtime_error
                              (sprintf "Cannot set attribute `%s`: DELETE_ATTR" id)))
         | DELETE_SUBSCR ->
             let tos = s_pop () in    (* del TOS1[TOS] *)
             let tos1 = s_pop () in
             (match tos1 with
                Str s ->
                  raise (Runtime_error "Strings do not support item deletion: DELETE_SUBSCR")
              | List darr ->
                  let idx = subscr_get_idx (as_int tos) (D.length darr) in
                  D.delete darr idx
              | Tuple arr ->
                  raise (Runtime_error "Tuples do not support item deletion: DELETE_SUBSCR")
              | Dict htbl ->
                  (match H.find_opt htbl tos with
                     Some v -> H.remove htbl tos
                   | None ->
                       raise (Runtime_error "Key not found: DELETE_SUBSCR"))
              | _ ->
                  raise (Runtime_error "Object not subscriptable: DELETE_SUBSCR"))
         | DELETE_SLICESUB ->
             let tos = s_pop () in     (* del TOS2[TOS1:TOS] *)
             let tos1 = s_pop () in
             let tos2 = s_pop () in
             (match tos2 with
                Str s ->
                  raise (Runtime_error "Strings do not support item deletion: DELETE_SLICESUB")
              | List darr ->
                  let n = D.length darr in
                  let i = pos_idx (as_int tos1) n in
                  D.delete_range darr i (sub_len i (as_int tos) n)
              | Tuple arr ->
                  raise (Runtime_error "Tuples do not support item deletion: DELETE_SLICESUB")
              | _ -> raise (Runtime_error "Object not slice-subscriptable: SLICESUB"))
         | STORE_SUBSCR ->
             let tos = s_pop () in     (* TOS1[TOS] <- TOS2 *)
             let tos1 = s_pop () in
             let tos2 = s_pop () in
             (match tos1 with
                Str s ->
                  raise (Runtime_error "Strings do not support item assignment: STORE_SUBSCR")
              | List darr ->
                  let idx = subscr_get_idx (as_int tos) (D.length darr) in
                  D.set darr idx tos2
              | Tuple arr ->
                  raise (Runtime_error "Tuples do not support item assignment: STORE_SUBSCR")
              | Dict htbl ->
                  (match H.find_opt htbl tos with
                     Some v -> H.replace htbl tos tos2
                   | None -> H.add htbl tos tos2)
              | _ ->
                  raise (Runtime_error "Object not subscriptable: STORE_SUBSCR"))
         | STORE_SLICESUB ->
             let tos = s_pop () in     (* TOS2[TOS1:TOS] <- TOS3 *)
             let tos1 = s_pop () in
             let tos2 = s_pop () in
             let tos3 = s_pop () in
             (match tos2 with
                Str s ->
                  raise (Runtime_error "Strings do not support item assignment: STORE_SLICESUB")
              | List darr ->
                  let n = D.length darr in
                  let i = pos_idx (as_int tos1) n in
                  (* delete old values in the slice *)
                  D.delete_range darr i (sub_len i (as_int tos) n);
                  (match tos3 with
                     List new_darr ->
                       for k = 0 to D.length new_darr - 1 do
                         D.insert darr (i + k) (D.get new_darr k)
                       done
                   | Tuple new_arr ->
                       for k = 0 to Array.length new_arr - 1 do
                         D.insert darr (i + k) new_arr.(k)
                       done
                   | Str str ->
                       for k = 0 to String.length str - 1 do
                         D.insert darr (i + k) (Str (String.make 1 str.[k]))
                       done
                   | Seq seq ->
                       let module Iter =
                         struct
                           exception Break
                         end
                       in
                       let ptr = ref seq in
                       let idx = ref i in
                       while true do
                         (match (!ptr) () with
                            Seq.Nil -> raise Iter.Break
                          | Seq.Cons(next, rest) ->
                              ptr := rest;
                              D.insert darr !idx next;
                              incr idx)
                       done
                   | _ -> raise (Runtime_error "Can only assign from iterable: STORE_SLICESUB"))
              | Tuple arr ->
                  raise (Runtime_error "Tuples do not support item assignment: STORE_SLICESUB")
              | _ -> raise (Runtime_error "Object not slice-subscriptable: STORE_SLICESUB"))
        );
        program_counter := !next
      done;
      (* default behaviour when no RETURN_VALUE is encountered is to return TOS *)
      if S.is_empty stack then Py_val.None else s_pop ()
    )
    with
      Loop.Exit pv -> pv    (* break encountered *)
    | Runtime_error s ->
        printf "Runtime error at bytecode `%s` line %d: %s\n" block_name !program_counter s;
        None
    | Py_val.Type_error ->
        printf "Type error at bytecode `%s` line %d.\n" block_name !program_counter;
        None
  in
  (* start interpreting from the top of instructions *)
  loop ()

(* interpret bytecode instructions, printing result when appropriate *)
let interpret c envr =
  let ret_val : Py_val.t = run c "MAIN" envr in
  match ret_val with
    None -> envr
  | _ ->
    (* try to print top of stack *)
    let pv = lookup_global envr "print" in
    (match pv with
       Fun (_, f) ->
         f [ret_val] |> ignore;
         envr
     | _ -> envr)

(* create a new environment and fill it with built-ins *)
let init_env () : env =
  (* initialise scopes -- two for now *)
  let (built_in_s : scope) = Built_in.table in
  let (global_s : scope) = H.create 5 in
  { cls = None;
    locals = [];
    globals = [global_s; built_in_s];
  }

