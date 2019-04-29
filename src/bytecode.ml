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

(* convert an expression to bytecode and add instructions to array *)
let rec compile_expr (arr : code) (e : Ast.expr) : unit =
  match e with
    Var id         -> D.add arr (LOAD_NAME id)
  | IntLit i       -> D.add arr (LOAD_CONST (Int i))
  | FloatLit f     -> D.add arr (LOAD_CONST (Float f))
  | BoolLit b      -> D.add arr (LOAD_CONST (Bool b))
  | StrLit s       -> D.add arr (LOAD_CONST (Str s))
  | Call (f, args) -> compile_expr arr f;
                      List.iter (compile_expr arr) args;
                      D.add arr (CALL_FUNCTION (List.length args))
  | Op (And, args) -> (match args with
                         e1::e2::_ ->
                           compile_expr arr e1;
                           let pop_index = D.length arr in
                           D.add arr (JUMP_IF_FALSE_OR_POP (-1)); (* dummy *)
                           compile_expr arr e2;
                           D.set arr pop_index (JUMP_IF_FALSE_OR_POP (D.length arr)) (* backfill *)
                       | _         -> raise (Bytecode_error "Not enough arguments: AND"))
  | Op (Or, args)  -> (match args with
                         e1::e2::_ ->
                           compile_expr arr e1;
                           let pop_index = D.length arr in
                           D.add arr (JUMP_IF_TRUE_OR_POP (-1)); (* dummy *)
                           compile_expr arr e2;
                           D.set arr pop_index (JUMP_IF_TRUE_OR_POP (D.length arr)) (* backfill *)
                       | _         -> raise (Bytecode_error "Not enough arguments: OR"))
  | Op (o, args)   -> List.iter (compile_expr arr) args;
                      (match o with
                       | Not    -> D.add arr UNARY_NOT
                       | Is     -> D.add arr COMPARE_IS
                       | In     -> D.add arr COMPARE_IN
                       | NotIn  -> D.add arr COMPARE_IN
                       | IsNot  -> D.add arr COMPARE_IS_NOT
                       | Plus   -> D.add arr BINARY_ADD
                       | Minus  -> D.add arr BINARY_SUB
                       | Times  -> D.add arr BINARY_MULT
                       | FpDiv  -> D.add arr BINARY_FP_DIV
                       | IntDiv -> D.add arr BINARY_INT_DIV
                       | Mod    -> D.add arr BINARY_MOD
                       | Exp    -> D.add arr BINARY_EXP
                       | Eq     -> D.add arr COMPARE_EQ
                       | Neq    -> D.add arr COMPARE_NEQ
                       | Lt     -> D.add arr COMPARE_LT
                       | Gt     -> D.add arr COMPARE_GT
                       | Leq    -> D.add arr COMPARE_LEQ
                       | Geq    -> D.add arr COMPARE_GEQ
                       | BwAnd  -> D.add arr BINARY_BW_AND
                       | BwOr   -> D.add arr BINARY_BW_OR
                       | BwComp -> D.add arr UNARY_BW_COMP
                       | BwXor  -> D.add arr BINARY_BW_XOR
                       | LShift -> D.add arr BINARY_LSHIFT
                       | RShift -> D.add arr BINARY_RSHIFT
                       | Neg    -> D.add arr UNARY_NEG
                       | _      -> raise (Bytecode_error "Invalid operator encountered."))
  | Cond (c,e1,e2) -> compile_expr arr c;
                      let pop_index = D.length arr in
                      D.add arr (POP_JUMP_IF_FALSE (-1)); (* dummy 1 *)
                      compile_expr arr e1;
                      let jump_index = D.length arr in
                      D.add arr (JUMP (-1)); (* dummy 2 *)
                      D.set arr pop_index (POP_JUMP_IF_FALSE (D.length arr)); (* backfill 1 *)
                      compile_expr arr e2;
                      D.set arr jump_index (JUMP (D.length arr)) (* backfill 2 *)
  | None           -> D.add arr (LOAD_CONST None)

(* convert a statement to bytecode and append instructions to array *)
let rec compile_stmt (arr : code) (in_loop : bool) (s : Ast.stmt) : unit =
  (match s with
     Expr e         -> compile_expr arr e;
(*
                       (match e with
                          Call _ -> D.add arr POP_TOP (* throw away result of call *)
                        | _      -> ())
*)
   | Assign (s, e)  -> compile_expr arr e;
                       D.add arr (STORE_NAME s);
   | If (c, s1, s2) -> compile_expr arr c;
                       let pop_index = D.length arr in
                       D.add arr (POP_JUMP_IF_FALSE (-1)); (* dummy 1 *)
                       List.iter (compile_stmt arr in_loop) s1;
                       (match s2 with
                          Some ss ->
                            let jump_index = D.length arr in
                            D.add arr (JUMP (-1)); (* dummy 2 *)
                            D.set arr pop_index (POP_JUMP_IF_FALSE (D.length arr)); (* backfill 1 *)
                            List.iter (compile_stmt arr in_loop) ss;
                            D.set arr jump_index (JUMP (D.length arr)); (* backfill 2 *)
                        | None ->
                            D.set arr pop_index (POP_JUMP_IF_FALSE (D.length arr))); (* just backfill 1 *)
   | While (c, ss)  -> let start_idx = D.length arr in
                       compile_expr arr c;
                       let pop_index = D.length arr in
                       D.add arr (POP_JUMP_IF_FALSE (-1)); (* dummy *)
                       List.iter (compile_stmt arr true) ss;
                       D.add arr (JUMP start_idx); (* goto beginning of loop *)
                       let end_idx = D.length arr in
                       D.set arr pop_index (POP_JUMP_IF_FALSE end_idx); (* backfill *)
                       (* scan over tokens that were added to backfill breaks and continues *)
                       for i = start_idx to end_idx - 1 do
                         match D.get arr i with
                           JUMP t -> if t = -10 then D.set arr i (JUMP end_idx) else
                                     if t = -20 then D.set arr i (JUMP start_idx) else ()
                         | _      -> ()
                       done
   | Break          -> if not in_loop then
                         raise (Bytecode_error "BREAK statement found outside loop.")
                       else
                         D.add arr (JUMP (-10)) (* -10 indicates break *)
   | Continue       -> if not in_loop then
                         raise (Bytecode_error "CONTINUE statement found outside loop.")
                       else
                         D.add arr (JUMP (-20))) (* -20 indicates continue *)

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
  let rec resolve_iter (stmts : Ast.stmt list) =
    match stmts with
      []    -> ()
    | s::ss -> (match s with
                  Expr e        -> resolve_expr e;
                | Assign(s, e)  -> H.replace table s (Local 0);
                                   resolve_expr e
                | If(c, s1, s2) -> resolve_expr c;
                                   resolve_iter s1;
                                   (match s2 with
                                      Some ss -> resolve_iter ss
                                    | None -> ())
                | While(c, s)   -> resolve_expr c;
                                   resolve_iter s
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
               resolve_iter ss
  in
  (* iterate compile statements *)
  let rec compile_iter arr stmts =
    match stmts with
      []    -> ()
    | s::ss -> compile_stmt arr false s;
               compile_iter arr ss in
  let instrs = D.create () in
  compile_iter instrs stmts;
(*
  D.add instrs (LOAD_CONST None);
  D.add instrs RETURN_VALUE;
*)
  instrs

(* interface to the rest of the system *)
let compile_prog (p : Ast.program) : code = compile_stmts p []
