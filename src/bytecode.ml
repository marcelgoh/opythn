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
type local_table = (string, depth) H.t

(* prints a list of instructions in readable format *)
let print_asm instrs =
  for i = 0 to D.length instrs - 1 do
    printf "%d\t%s\n" i (Instr.str_of_instr (D.get instrs i))
  done


(* recursively compile statements *)
let rec compile_stmts stmts sym_scope =

  let is_global = match sym_scope with [] -> true | _ -> false in
  let table : local_table = H.create 10 in

  (* check enclosing scopes for name, return number of levels up *)
  let rec search_upwards count tables str : int option =
    match sym_scope with
      []    -> None
    | t::ts -> (match H.find_opt t str with
                  Some d -> (match d with
                               Local _ -> Some count
                             | _       -> None)
                | None   -> search_upwards (count + 1) ts str)
  in

  (* resolve a single expr and return unit *)
  let rec resolve_expr (expr : Ast.expr) =
    match expr with
      Var s           -> H.replace table s Referred;
    | Call(f ,es)     -> resolve_expr f;
                         List.iter resolve_expr es
    | Op(_, e)        -> List.iter resolve_expr e
    | Cond(e1, c, e2) -> resolve_expr e1;
                         resolve_expr c;
                         resolve_expr e2
    | IntLit _ | FloatLit _ | BoolLit _ | StrLit _ | Lambda(_,_) | None -> ()
  in

  (* iterate through statements and resolve each one *)
  let rec resolve_stmts (stmts : Ast.stmt list) =
    match stmts with
      []    -> ()
    | s::ss -> (match s with
                  Expr e        -> resolve_expr e;
                | Assign(s, e)  -> H.replace table s (Local 0);
                                   resolve_expr e
                | If(c, s1, s2) -> resolve_expr c;
                                   resolve_stmts s1;
                                   (match s2 with
                                      Some ss -> resolve_stmts ss
                                    | None -> ())
                | While(c, s)   -> resolve_expr c;
                                   resolve_stmts s
                | Global s      -> (match H.find_opt table s with
                                      None   -> H.add table s Global
                                    | Some _ ->
                                        if is_global then ()
                                        else raise (Bytecode_error
                                                      (sprintf "Name \"%s\" used before global declaration" s)))
                | Nonlocal s    -> if is_global then
                                     raise (Bytecode_error "Nonlocal declared in global scope")
                                   else (match search_upwards 1 sym_scope s with
                                           Some num -> H.add table s (Local num)
                                         | None -> raise (Bytecode_error
                                                            (sprintf "No binding for %s found" s)))
                | Return e      -> resolve_expr e
                | Funcdef(_,_,_) | Break | Continue -> ());
               resolve_stmts ss
  in

  let instrs : Instr.t D.t = D.create () in  (* the two functions below modify this instruction array *)

  (* convert an expression to bytecode and add instructions to array *)
  let rec compile_expr (e : Ast.expr) : unit =
    match e with
      Var id         -> let instr : Instr.t =
                          (match H.find_opt table id with
                             Some (Local i) -> (LOAD_LOCAL(i, id))
                           | Some Global
                           | Some Referred  -> (LOAD_GLOBAL id)
                           | None -> (LOAD_NAME id)) (* this should not happen *)
                        in
                        D.add instrs instr
    | IntLit i       -> D.add instrs (LOAD_CONST (Int i))
    | FloatLit f     -> D.add instrs (LOAD_CONST (Float f))
    | BoolLit b      -> D.add instrs (LOAD_CONST (Bool b))
    | StrLit s       -> D.add instrs (LOAD_CONST (Str s))
    | Call (f, args) -> compile_expr f;
                        List.iter compile_expr args;
                        D.add instrs (CALL_FUNCTION (List.length args))
    | Op (And, args) -> (match args with
                           e1::e2::_ ->
                             compile_expr e1;
                             let pop_index = D.length instrs in
                             D.add instrs (JUMP_IF_FALSE_OR_POP (-1)); (* dummy *)
                             compile_expr e2;
                             D.set instrs pop_index (JUMP_IF_FALSE_OR_POP (D.length instrs)) (* backfill *)
                         | _         -> raise (Bytecode_error "Not enough arguments: AND"))
    | Op (Or, args)  -> (match args with
                           e1::e2::_ ->
                             compile_expr e1;
                             let pop_index = D.length instrs in
                             D.add instrs (JUMP_IF_TRUE_OR_POP (-1)); (* dummy *)
                             compile_expr e2;
                             D.set instrs pop_index (JUMP_IF_TRUE_OR_POP (D.length instrs)) (* backfill *)
                         | _         -> raise (Bytecode_error "Not enough arguments: OR"))
    | Op (o, args)   -> List.iter compile_expr args;
                        (match o with
                         | Not    -> D.add instrs UNARY_NOT
                         | Is     -> D.add instrs COMPARE_IS
                         | In     -> D.add instrs COMPARE_IN
                         | NotIn  -> D.add instrs COMPARE_IN
                         | IsNot  -> D.add instrs COMPARE_IS_NOT
                         | Plus   -> D.add instrs BINARY_ADD
                         | Minus  -> D.add instrs BINARY_SUB
                         | Times  -> D.add instrs BINARY_MULT
                         | FpDiv  -> D.add instrs BINARY_FP_DIV
                         | IntDiv -> D.add instrs BINARY_INT_DIV
                         | Mod    -> D.add instrs BINARY_MOD
                         | Exp    -> D.add instrs BINARY_EXP
                         | Eq     -> D.add instrs COMPARE_EQ
                         | Neq    -> D.add instrs COMPARE_NEQ
                         | Lt     -> D.add instrs COMPARE_LT
                         | Gt     -> D.add instrs COMPARE_GT
                         | Leq    -> D.add instrs COMPARE_LEQ
                         | Geq    -> D.add instrs COMPARE_GEQ
                         | BwAnd  -> D.add instrs BINARY_BW_AND
                         | BwOr   -> D.add instrs BINARY_BW_OR
                         | BwComp -> D.add instrs UNARY_BW_COMP
                         | BwXor  -> D.add instrs BINARY_BW_XOR
                         | LShift -> D.add instrs BINARY_LSHIFT
                         | RShift -> D.add instrs BINARY_RSHIFT
                         | Neg    -> D.add instrs UNARY_NEG
                         | _      -> raise (Bytecode_error "Invalid operator encountered."))
    | Cond (c,e1,e2) -> compile_expr c;
                        let pop_index = D.length instrs in
                        D.add instrs (POP_JUMP_IF_FALSE (-1)); (* dummy 1 *)
                        compile_expr e1;
                        let jump_index = D.length instrs in
                        D.add instrs (JUMP (-1)); (* dummy 2 *)
                        D.set instrs pop_index (POP_JUMP_IF_FALSE (D.length instrs)); (* backfill 1 *)
                        compile_expr e2;
                        D.set instrs jump_index (JUMP (D.length instrs)) (* backfill 2 *)
    | None           -> D.add instrs (LOAD_CONST None)
  in

  (* convert a statement to bytecode and append instructions to instruction array *)
  let rec compile_stmt (in_loop : bool) (s : Ast.stmt) : unit =
    (match s with
       Expr e         -> compile_expr e;
     | Assign (s, e)  -> compile_expr e;
                         let instr : Instr.t =
                           (match H.find_opt table s with
                              Some (Local i) -> (STORE_LOCAL(i, s))
                            | Some Global
                            | Some Referred  -> (STORE_GLOBAL s)
                            | None -> (STORE_NAME s)) (* this should not happen *)
                         in
                         D.add instrs instr
     | If (c, s1, s2) -> compile_expr c;
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
     | While (c, ss)  -> let start_idx = D.length instrs in
                         compile_expr c;
                         let pop_index = D.length instrs in
                         D.add instrs (POP_JUMP_IF_FALSE (-1)); (* dummy *)
                         List.iter (compile_stmt true) ss;
                         D.add instrs (JUMP start_idx); (* goto beginning of loop *)
                         let end_idx = D.length instrs in
                         D.set instrs pop_index (POP_JUMP_IF_FALSE end_idx); (* backfill *)
                         (* scan over tokens that were added to backfill breaks and continues *)
                         for i = start_idx to end_idx - 1 do
                           match D.get instrs i with
                             JUMP t -> if t = -10 then D.set instrs i (JUMP end_idx) else
                                       if t = -20 then D.set instrs i (JUMP start_idx) else ()
                           | _      -> ()
                         done
     | Break          -> if not in_loop then
                           raise (Bytecode_error "BREAK statement found outside loop.")
                         else
                           D.add instrs (JUMP (-10)) (* -10 indicates break *)
     | Continue       -> if not in_loop then
                           raise (Bytecode_error "CONTINUE statement found outside loop.")
                         else
                           D.add instrs (JUMP (-20))) (* -20 indicates continue *)
  in

  (* iterate and compile statements *)
  let rec compile_iter stmts =
    match stmts with
      []    -> ()
    | s::ss -> compile_stmt false s;
               compile_iter ss
  in

  (* start routine *)
  resolve_stmts stmts;   (* fill the hashtable of depths *)
  compile_iter stmts;    (* compile *)
(*
  D.add instrs (LOAD_CONST None);
  D.add instrs RETURN_VALUE;
*)
  instrs

(* interface to the rest of the system *)
let compile_prog (p : Ast.program) : code = compile_stmts p []
