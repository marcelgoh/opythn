(* Bytecode representation and compilation *)

open Printf
module D = DynArray
module H = Hashtbl

exception Bytecode_error of string

(* alias for a DynArray of Instr.t *)
type code = Instr.t DynArray.t

(* representation for how deeply nested a variable can be *)
type depth =
  Local of int
| Global
| Referred

(* table of names and depths for a given scope *)
type sym_table = (string, depth) H.t

(* print a string and its depth *)
(* usage: H.iter print_entry (table : sym_table) *)
let print_entry str depth =
  let dstr =
    match depth with
      Local i -> sprintf "Local %d" i
    | Global -> "Global" | Referred -> "Referred"
  in
  printf "%s: %s\n" str dstr

(* print bytecode *)
let print_asm = Instr.print_instr_array

(* recursively compile statements *)
let rec compile_stmts in_repl in_class stmts enclosings table =

  let is_global = enclosings = [] in

  (* where nonlocals are stored before they're actually used *)
  let nonlocals = H.create 10 in

  (* for variables directly in class definitions *)
  let class_vars = H.create 10 in

  (* check both non-local and local tables *)
  let check_tables_opt str =
    match H.find_opt table str with
      Some depth -> Some depth
    | None ->
        (match H.find_opt nonlocals str with
           Some d -> Some d
         | None -> None)
  in

  (* check enclosing scopes for name, return number of levels up
   * calling sequence should set count to 1
   *)
  let rec search_upwards count tables str : int option =
    match tables with
      []    -> None
    | t::ts -> (match H.find_opt t str with
                  Some d -> (match d with
                              Local _ -> Some count
                             | _ -> None)
                | None   -> search_upwards (count + 1) ts str)
  in

  (* resolve a single expr and return unit *)
  let rec resolve_expr (expr : Ast.expr) =
    match expr with
      Var s -> (match check_tables_opt s with
                  Some _ -> ()
                | None -> H.replace table s Referred)
    | Call(f ,es) -> resolve_expr f;
                     List.iter resolve_expr es
    | Op(_, e) -> List.iter resolve_expr e
    | Cond(e1, c, e2) -> resolve_expr e1;
                         resolve_expr c;
                         resolve_expr e2
    | AttrRef(obj, _) -> resolve_expr obj
    | Subscr(seq, i, slice) ->
        (match slice with
           Some j -> resolve_expr j
         | None -> ()
        );
        resolve_expr i;
        resolve_expr seq;
    | ListLit _ | DictLit _ | TupleLit _
    | IntLit _ | FloatLit _ | BoolLit _ | StrLit _ | Lambda(_, _) | None -> ()
  in

  (* iterate through statements and resolve each one *)
  let rec resolve_stmts (stmts : Ast.stmt list) =
    let add_name name =
      if in_class then
        H.replace class_vars name (Local 0)
      else
        let depth_to_add = if is_global then Global else (Local 0) in
          match check_tables_opt name with
            (* the only time a nonlocal shifts over to local table *)
            Some d -> H.replace table name d
          | None -> H.replace table name depth_to_add
    in
    match stmts with
      [] -> ()
    | s::ss ->
        (match s with
           Expr e -> resolve_expr e;
         | Assign(Var s, e) ->
             resolve_expr e;
             (match H.find_opt table s with
                Some Referred ->
                  if not in_repl then
                    raise (Bytecode_error (sprintf "Local name `%s` used before assignment" s))
                  else
                    ()
              | _ -> ());
             add_name s
         | Assign(AttrRef(obj, _), e) ->
             resolve_expr e;
             (* we don't resolve the identifier *)
             resolve_expr obj
         | Assign(_, _) ->
             raise (Bytecode_error "Tried to assign to non-assignable expression")
         | If(c, s1, s2) ->
             resolve_expr c;
             resolve_stmts s1;
             (match s2 with
                Some ss -> resolve_stmts ss
              | None -> ())
         | While(c, s) ->
             resolve_expr c;
             resolve_stmts s
         | For(id, seq, s) ->
             resolve_expr id;
             resolve_expr seq;
             resolve_stmts s
         | Global s ->
             if in_class then
               H.replace class_vars s Global
             else
               (match check_tables_opt s with
                  None   -> H.replace table s Global
                | Some _ ->
                    if is_global then ()
                    else
                      raise
                      (Bytecode_error (sprintf "Name `%s` used before global declaration" s)))
         | Nonlocal s ->
             if is_global then
               raise (Bytecode_error "Nonlocal declared in global scope")
             else
               if in_class then
                 (match H.find_opt table s with
                    Some _ -> H.replace nonlocals s (Local 0)
                  | None ->
                      (match search_upwards 1 enclosings s with
                         Some num -> H.replace nonlocals s (Local num)
                       | None ->
                           raise (Bytecode_error (sprintf "No binding for %s found" s))))
               else
                 (match H.find_opt table s with
                    Some _ ->
                      raise
                        (Bytecode_error (sprintf "Name `%s` used before nonlocal declaration" s))
                  | None ->
                      (match search_upwards 1 enclosings s with
                         Some num -> H.replace nonlocals s (Local num)
                       | None ->
                           raise (Bytecode_error (sprintf "No binding for %s found" s))))
         | Return e -> resolve_expr e
         | Del e -> resolve_expr e
         (* we don't resolve inside function or class declarations *)
         | Funcdef(name, _, _) ->
             add_name name
         | Classdef(name, _, _) ->
             add_name name
         | Break | Continue | Pass -> ());
        resolve_stmts ss
  in

  (* the two functions below modify this instruction array *)
  let instrs : Instr.t D.t = D.create () in

  (* helper function: search list of list of lambda arguments
   * calling sequence should set count to 0
   *)
  let rec search_lambdas count lambdas id =
    match lambdas with
      []    -> None
    | l::ls -> if List.mem id l then Some count
               else search_lambdas (count + 1) ls id
  in
  (* helper function to get load instruction from identifier *)
  let get_load_instr lambdas id : Instr.t =
    let search : Instr.t =
      (match search_lambdas 0 lambdas id with
        Some n -> (LOAD_LOCAL(n, id))
      | None ->
          (match check_tables_opt id with
             Some (Local i) -> (LOAD_LOCAL(i + List.length lambdas, id))
           | Some Global -> (LOAD_GLOBAL id)
           | Some Referred ->
               (match search_upwards 1 enclosings id with
                  Some n -> (LOAD_LOCAL(n, id))
                | None -> (LOAD_GLOBAL id))
           | None ->
               raise (Bytecode_error (sprintf "Variable `%s`'s scope not resolved: COMPILE_EXPR" id))))
    in
    if in_class then
      (match H.find_opt class_vars id with
         Some d ->
           (match d with
              Global -> (LOAD_GLOBAL id)  (* explicit global *)
            | Referred | Local _ -> LOAD_NAME id)
       | None ->
           let instr = search in
           (match instr with
              LOAD_LOCAL (i, _) ->
                LOAD_LOCAL (i, id)
            | LOAD_GLOBAL _ ->
                LOAD_NAME id
            | _ ->
                raise (Bytecode_error
                         (sprintf "Failed to get load instruction for `%s`: VAR" id))))
    else
      search
  in
  (* convert an expression to bytecode and add instructions to array *)
  let rec compile_expr (lambdas : string list list) (e : Ast.expr) : code =
    (* local instruction array for compiling expressions *)
    let expr_instrs = D.create () in
    let compile_and_add_expr expr = D.append (compile_expr lambdas expr) expr_instrs in
    (* pattern match expression and compile accordingly *)
    (match e with
       Var id ->
           let instr : Instr.t = get_load_instr lambdas id in
           D.add expr_instrs instr
     | IntLit i -> D.add expr_instrs (LOAD_CONST (Int i))
     | FloatLit f -> D.add expr_instrs (LOAD_CONST (Float f))
     | BoolLit b -> D.add expr_instrs (LOAD_CONST (Bool b))
     | StrLit s -> D.add expr_instrs (LOAD_CONST (Str s))
     | Call (f, args) ->
         compile_and_add_expr f;
         List.iter compile_and_add_expr args;
         D.add expr_instrs (CALL_FUNCTION (List.length args))
     | Op (And, args) ->
         (match args with
            e1::e2::_ ->
              compile_and_add_expr e1;
              let pop_index = D.length expr_instrs in
              D.add expr_instrs (JUMP_IF_FALSE_OR_POP (-1)); (* dummy *)
              compile_and_add_expr e2;
              D.set expr_instrs pop_index (JUMP_IF_FALSE_OR_POP (D.length expr_instrs)) (* backfill *)
          | _ -> raise (Bytecode_error "Not enough arguments: AND"))
     | Op (Or, args) ->
         (match args with
            e1::e2::_ ->
              compile_and_add_expr e1;
              let pop_index = D.length expr_instrs in
              D.add expr_instrs (JUMP_IF_TRUE_OR_POP (-1)); (* dummy *)
              compile_and_add_expr e2;
              D.set expr_instrs pop_index (JUMP_IF_TRUE_OR_POP (D.length expr_instrs)) (* backfill *)
          | _ -> raise (Bytecode_error "Not enough arguments: OR"))
     | Op (o, args) ->
         List.iter compile_and_add_expr args;
         (match o with
          | Not    -> D.add expr_instrs UNARY_NOT
          | Is     -> D.add expr_instrs COMPARE_IS
          | In     -> D.add expr_instrs COMPARE_IN
          | NotIn  -> D.add expr_instrs COMPARE_IN
          | IsNot  -> D.add expr_instrs COMPARE_IS_NOT
          | Plus   -> D.add expr_instrs BINARY_ADD
          | Minus  -> D.add expr_instrs BINARY_SUB
          | Times  -> D.add expr_instrs BINARY_MULT
          | FpDiv  -> D.add expr_instrs BINARY_FP_DIV
          | IntDiv -> D.add expr_instrs BINARY_INT_DIV
          | Mod    -> D.add expr_instrs BINARY_MOD
          | Exp    -> D.add expr_instrs BINARY_EXP
          | Eq     -> D.add expr_instrs COMPARE_EQ
          | Neq    -> D.add expr_instrs COMPARE_NEQ
          | Lt     -> D.add expr_instrs COMPARE_LT
          | Gt     -> D.add expr_instrs COMPARE_GT
          | Leq    -> D.add expr_instrs COMPARE_LEQ
          | Geq    -> D.add expr_instrs COMPARE_GEQ
          | BwAnd  -> D.add expr_instrs BINARY_BW_AND
          | BwOr   -> D.add expr_instrs BINARY_BW_OR
          | BwComp -> D.add expr_instrs UNARY_BW_COMP
          | BwXor  -> D.add expr_instrs BINARY_BW_XOR
          | LShift -> D.add expr_instrs BINARY_LSHIFT
          | RShift -> D.add expr_instrs BINARY_RSHIFT
          | Neg    -> D.add expr_instrs UNARY_NEG
          | _      -> raise (Bytecode_error "Invalid operator encountered."))
     | Cond(c,e1,e2) ->
         compile_and_add_expr c;
         let pop_index = D.length expr_instrs in
         D.add expr_instrs (POP_JUMP_IF_FALSE (-1)); (* dummy 1 *)
         compile_and_add_expr e1;
         let jump_index = D.length expr_instrs in
         D.add expr_instrs (JUMP (-1)); (* dummy 2 *)
         D.set expr_instrs pop_index (POP_JUMP_IF_FALSE (D.length expr_instrs)); (* backfill 1 *)
         compile_and_add_expr e2;
         D.set expr_instrs jump_index (JUMP (D.length expr_instrs)) (* backfill 2 *)
     | Lambda(args,b) ->
         let new_table = H.create 10 in
         List.iter (fun s -> H.replace new_table s (Local 0)) args;
         let code_block = compile_expr (args :: lambdas) b in
         D.add code_block RETURN_VALUE;
         D.add expr_instrs (MAKE_FUNCTION(args, { name = "<lambda>";
                                                  ptr = ref code_block }))
     | AttrRef(e, id) ->
         compile_and_add_expr e;
         D.add expr_instrs (LOAD_ATTR id)
     | Subscr(seq, i, slice) ->
         compile_and_add_expr seq;
         compile_and_add_expr i;
         (match slice with
            Some j ->
              compile_and_add_expr j;
              D.add expr_instrs SLICESUB
          | None -> D.add expr_instrs SUBSCR)
     | ListLit elems ->
         List.iter (fun e -> compile_and_add_expr e) elems;
         D.add expr_instrs (BUILD_LIST (List.length elems))
     | TupleLit elems ->
         List.iter (fun e -> compile_and_add_expr e) elems;
         D.add expr_instrs (BUILD_TUPLE (List.length elems))
     | DictLit elems ->
         List.iter (fun (k,v) -> compile_and_add_expr k; compile_and_add_expr v) elems;
         D.add expr_instrs (BUILD_DICT (List.length elems))
     | None -> D.add expr_instrs (LOAD_CONST None)
    );
    expr_instrs
  in

  (* convert a statement to bytecode and append instructions to instruction array *)
  let rec compile_stmt (in_loop : bool) (s : Ast.stmt) : unit =
    let compile_and_add_expr expr = D.append (compile_expr [] expr) instrs in
    let get_store_instr id : Instr.t =
      if in_class then
        STORE_NAME id
      else
        match check_tables_opt id with
          Some (Local n) -> (STORE_LOCAL(n, id))
        | Some Global
        | Some Referred  -> (STORE_GLOBAL id)
        | None -> raise (Bytecode_error (sprintf "Variable `%s`'s scope not resolved: GET_STORE_INSTR" id))
    in
    let compile_loop start_idx pop_idx stmts for_var =
      if for_var <> "" then D.add instrs (get_store_instr for_var) else ();
      List.iter (compile_stmt true) stmts;
      D.add instrs (JUMP start_idx); (* goto beginning of loop *)
      let end_idx = D.length instrs in
      (* backfill dummy address *)
      if for_var <> "" then (
        D.set instrs pop_idx (FOR_ITER end_idx);
      )
      else
        D.set instrs pop_idx (POP_JUMP_IF_FALSE end_idx);
      (* scan over tokens that were added to backfill breaks and continues *)
      for i = start_idx to end_idx - 1 do
        match D.get instrs i with
          JUMP t -> if t = -10 then D.set instrs i (JUMP end_idx) else
                    if t = -20 then D.set instrs i (JUMP start_idx) else ()
        | _ -> ()
      done
    in
    (match s with
       Expr e -> compile_and_add_expr e;
     | Assign (Var id, e) ->
         compile_and_add_expr e;
         D.add instrs (get_store_instr id)
     | Assign (AttrRef(obj, id), e) ->
         compile_and_add_expr e;
         compile_and_add_expr obj;
         D.add instrs (STORE_ATTR id)
     | Assign(_, _) ->
         raise (Bytecode_error "Tried to assign to non-assignable expression")
     | If (c, s1, s2) ->
         compile_and_add_expr c;
         let pop_index = D.length instrs in
         D.add instrs (POP_JUMP_IF_FALSE (-1)); (* dummy 1 *)
         List.iter (compile_stmt in_loop) s1;
         (match s2 with
            Some ss ->
              let jump_index = D.length instrs in
              D.add instrs (JUMP (-1)); (* dummy 2 *)
              D.set instrs pop_index (POP_JUMP_IF_FALSE (D.length instrs)); (* backfill 1 *)
              List.iter (compile_stmt in_loop) ss;
              D.set instrs jump_index (JUMP (D.length instrs)); (* backfill 2 *)
          | None ->
              D.set instrs pop_index (POP_JUMP_IF_FALSE (D.length instrs))); (* just backfill 1 *)
     | While (c, ss) ->
         let start_idx = D.length instrs in
         compile_and_add_expr c;
         let pop_idx = D.length instrs in
         D.add instrs (POP_JUMP_IF_FALSE (-1)); (* dummy *)
         compile_loop start_idx pop_idx ss ""
     | For (Var id, seq, ss) ->
         compile_and_add_expr seq;
         D.add instrs BUILD_SEQ;
         let start_idx = D.length instrs in
         D.add instrs (FOR_ITER (-1));  (* dummy *)
         compile_loop start_idx start_idx ss id (* start and pop indices are the same *)
     | For _ ->
         raise (Bytecode_error "Cannot use provided iteration variable in for loop.")
     | Funcdef (name, args, body) ->
         let new_table = H.create 10 in
         List.iter (fun s -> H.replace new_table s (Local 0)) args;
         let code_block : code = compile_stmts in_repl false body (table::enclosings) new_table in
         D.add instrs (MAKE_FUNCTION (args, { name = name;
                                              ptr = ref code_block }));
         D.add instrs (get_store_instr name)
     | Return e ->
         compile_and_add_expr e;
         D.add instrs RETURN_VALUE
     | Del (Var id) ->
         (match get_load_instr [] (* not in lambda *) id with
            LOAD_LOCAL (n, id) -> D.add instrs (DELETE_LOCAL (n, id))
          | LOAD_GLOBAL id -> D.add instrs (DELETE_GLOBAL id)
          | LOAD_NAME id -> D.add instrs (DELETE_NAME id)
          | _ -> raise (Bytecode_error "Could not generate delete instruction"))
     | Del (Subscr(seq, i, slice)) ->
         compile_and_add_expr seq;
         compile_and_add_expr i;
         (match slice with
            Some j ->
              compile_and_add_expr j;
              D.add instrs SLICESUB
          | None -> D.add instrs SUBSCR)
     | Del (AttrRef(obj, id)) ->
         compile_and_add_expr obj;
         D.add instrs (DELETE_ATTR id)
     | Del _ -> raise (Bytecode_error "Cannot delete object.")
     | Break ->
         if not in_loop then
           raise (Bytecode_error "BREAK statement found outside loop.")
         else
           D.add instrs (JUMP (-10)) (* -10 indicates break *)
     | Continue ->
         if not in_loop then
           raise (Bytecode_error "CONTINUE statement found outside loop.")
         else
           D.add instrs (JUMP (-20)) (* -20 indicates continue *)
     | Classdef (name, super, body) ->
         let code_block = compile_stmts in_repl true body enclosings table in
         (* side effect: pushes superclass onto stack if needed *)
         let num_supers =
           match super with
             Some s -> D.add instrs (LOAD_NAME s);
                       1
           | None -> 0
         in
         D.add instrs (MAKE_CLASS (num_supers,
                                   { name = name; ptr = ref code_block }));
         D.add instrs (get_store_instr name)
     | Global _ | Nonlocal _ | Pass -> ()
    )
  in

  (* iterate and compile statements *)
  let rec compile_iter stmts =
    match stmts with
      [] -> ()
    | s::ss -> compile_stmt false s;
               compile_iter ss
  in

  (* start routine *)
  resolve_stmts stmts;   (* fill the hashtable of depths *)
(*   H.iter print_entry table; *)
  compile_iter stmts;    (* compile *)
  if D.last instrs <> RETURN_VALUE then (
    D.add instrs (LOAD_CONST None);
    D.add instrs RETURN_VALUE
  )
  else ();
  instrs

(* interface to the rest of the system *)
let compile_prog in_repl (p : Ast.program) : code =
  compile_stmts in_repl false p [] (H.create 10)
