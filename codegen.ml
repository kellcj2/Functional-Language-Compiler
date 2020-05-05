(*
Modified By: Christopher J. Kelly
Due Date:    4/8/2020
Assignment:  #6
Course:      Compilers II
Professor:   Schwesinger
Filename:    codegen.ml
Purpose:     Generates asm code using 'asm.ml'. Keeps track of stack
*)

open Ast
open Asm
open Printf

let e_intmask  = HexConst64(0x0000000000000001L)
let e_true     = HexConst64(0x0000000000000006L)
let e_false    = HexConst64(0x0000000000000002L)
let e_error    = HexConst64(0x0000000000000004L)

type type_tag =
  | TInt
  | TBool
  | TTup

let find_dup (bs : (string * expr) list) =
  let rec find_dup_helper lst b =
    match lst with
    | [] -> None
    | (name,_)::tail -> if (List.exists (fun x -> x = name) b)
                            then Some(name)
                            else find_dup_helper tail (name::b)
  in
  find_dup_helper bs []

let func_exists name num_params funs =
  List.exists (fun (x, y) -> (x = name && y = num_params)) funs

let stackloc i = RegOffset(-8 * i, RSP)
let save_to_stack i = [ IMov(Reg(RAX), stackloc i) ]
let restore_stack i = [ IMov(stackloc i, Reg(RAX)) ]

let flatmap  f xs = List.map  f xs |> List.flatten
let flatmapi f xs = List.mapi f xs |> List.flatten

let label_num = ref 0
let get_label_num() =
  label_num := !label_num + 1;
  !label_num


let type_mask ty =
  HexConst64 (match ty with
              | TInt  -> 0x1L
              | TBool -> 0x3L
              | TTup  -> 0x3L
    )

let type_tag ty =
  HexConst64 (match ty with
              | TInt  -> 0x1L
              | TBool -> 0x2L
              | TTup  -> 0x0L
    )

let typecheck ty =
  [ IAnd (type_mask ty, Reg(RAX))
  ; ICmp (type_tag ty, Reg(RAX))
  ]

let is_type ty =
  let label_true = "comp_true" ^ string_of_int (get_label_num()) in
  let label_end  = "comp_end" ^ string_of_int (get_label_num()) in
  typecheck ty @
  [ IJEQ label_true
  ; IMov(e_false, Reg(RAX))
  ; IJump label_end
  ; ILabel label_true
  ; IMov(e_true, Reg(RAX))
  ; ILabel label_end
  ]

let rec arith_op loc op : instruction list =
  [ ISar(Const(1), Reg(RAX)) (* shift operands right *)
  ; IMov(loc, Reg(R10))
  ; ISar(Const(1), Reg(R10))
  ; (match op with
     | Plus ->  IAdd(Reg(R10), Reg(RAX))
     | Minus -> ISub(Reg(R10), Reg(RAX))
     | Mult -> IMul(Reg(R10), Reg(RAX))
     | _ -> failwith "Not an arith operator"
    )
  ; IShl(Const(1), Reg(RAX)) (* shift result left *)
  ; IOr(Const(1), Reg(RAX)) (* set lsb to 1 *)
  ]

and check_int si : instruction list =
  let label_true = "no_error" ^ string_of_int (get_label_num()) in
  typecheck TInt @
  [ IJEQ label_true
  ; ISub(Const(8 * si), Reg(RSP))
  ; IMov(Const(-1), Reg(RDI)) (* error code *)
  ; ICall("builtin_error")
  ; IAdd (Const(8 * si), Reg(RSP))
  ; ILabel label_true
  ]

and compare_op loc op : instruction list =
  (* create branch names *)
  let false_branch = "false_branch" ^ string_of_int (get_label_num()) in
  let end_of_cmp = "end_of_cmp" ^ string_of_int (get_label_num()) in
  let jump_false = match op with
    | LessThan -> [ IJGTE(false_branch) ]
    | GreaterThan -> [ IJLTE(false_branch) ]
    | Equivalent -> [ IJNEQ(false_branch) ]
    | _ -> failwith "Not a comparison operator"
  in
  (* if comparison is true, move "true" to RAX, else move "false" to RAX *)
  [ ICmp(loc, Reg(RAX)) ] (* evaluate condition *)
  @ jump_false (* jump if false *)
  @ [ IMov(e_true, Reg(RAX)) ] (* move true to RAX *)
  @ [ IJump(end_of_cmp) ] (* go to end_of_if*)
  @ [ ILabel(false_branch) ] (* label for else *)
  @ [ IMov(e_false, Reg(RAX)) ] (* move "false" to RAX *)
  @ [ ILabel(end_of_cmp) ] (* end label *)

and check_equiv loc si : instruction list =
  (* this is dumb, but it works *)
  let not_same = "not_same" ^ string_of_int (get_label_num()) in
  let label_end = "l_end" ^ string_of_int (get_label_num()) in
  let label_bool = "l_bool" ^ string_of_int (get_label_num()) in
  let label_tup = "l_tup" ^ string_of_int (get_label_num()) in
  [ IMov(Reg(RAX), Reg(R10)) ] (* save vals in R10 and R11 *)
  @ [ IMov(loc, Reg(R11)) ]

  @ [ IAnd(type_mask TInt, Reg(R10)) ] (* if first op is int, check other *)
  @ [ ICmp(type_tag TInt, Reg(R10)) ]
  @ [ IJNEQ(label_bool) ]
  @ [ IAnd(type_mask TInt, Reg(R11)) ]
  @ [ ICmp(type_tag TInt, Reg(R11)) ]
  @ [ IJEQ(label_end) ]
  @ [ IJump(not_same) ]

  @ [ ILabel(label_bool) ] (* if first op is bool, check other*)
  @ [ IMov(Reg(RAX), Reg(R10)) ] 
  @ [ IAnd(type_mask TBool, Reg(R10)) ]
  @ [ ICmp(type_tag TBool, Reg(R10)) ]
  @ [ IJNEQ(label_tup) ]
  @ [ IAnd(type_mask TBool, Reg(R11)) ]
  @ [ ICmp(type_tag TBool, Reg(R11)) ]
  @ [ IJEQ(label_end) ]
  @ [ IJump(not_same) ]

  @ [ ILabel(label_tup) ] (* dont need to check R10, since it must be a tuple *)
  @ [ IAnd(type_mask TTup, Reg(R11)) ]
  @ [ ICmp(type_tag TTup, Reg(R11)) ]
  @ [ IJEQ(label_end) ]

  @ [ ILabel(not_same) ] (* not equivalent types *)
  @ [ ISub(Const(8 * si), Reg(RSP)) ]
  @ [ IMov(Const(-8), Reg(RDI)) ] (* error code *)
  @ [ ICall("builtin_error") ]
  @ [ IAdd (Const(8 * si), Reg(RSP)) ]

  @ [ ILabel(label_end) ] (* types are equivalent *)

and emit_binop (o : binop) (e1 : expr) (e2 : expr) (si : int) (scope : (string * int) list ) (funs:(string*int) list) : instruction list = 
  let loc = stackloc (si+1) in
  let e1_is = (emit_expr e1 si scope funs) @ save_to_stack si @ if o != Equivalent then (check_int si) else [] in
  let e2_is = (emit_expr e2 (si+1) scope funs) @ save_to_stack (si+1) @ if o != Equivalent then (check_int si) else [] in
  let op_is = match o with
    | Plus ->        arith_op loc Plus
    | Minus ->       arith_op loc Minus
    | Mult ->        arith_op loc Mult
    | LessThan ->    compare_op loc LessThan
    | GreaterThan -> compare_op loc GreaterThan
    | Equivalent ->  check_equiv loc si @ compare_op loc Equivalent
  in
  e1_is @ e2_is @ restore_stack si @ op_is

and emit_unop (u : unop) (e : expr) (si : int) (scope : (string * int) list) (funs:(string*int) list) : instruction list =
  let ex = emit_expr e si scope funs in
  let e_is = match u with
    | Print -> [ ISub(Const(8 * si), Reg(RSP)) ]
               @ [ IMov(Reg(RAX), Reg(RDI)) ]
               @ [ ICall("print") ]
               @ [ IAdd(Const(8 * si), Reg(RSP)) ]
    | IsBool -> is_type TBool
    | IsInt -> is_type TInt
  in ex @ e_is


and emit_let (bs : (string * expr) list) (body : expr) (si : int) (scope : (string * int) list) (funs:(string*int) list) : instruction list =
  match bs with
  | [] -> emit_expr body si scope funs
  | (x,e)::tail ->
          let e_is = emit_expr e si scope funs in
          e_is @ save_to_stack si @ emit_let tail body (si+1) ((x,si)::scope) funs


and emit_if (e_if : expr) (e_then : expr) (e_else : expr) (si :int) (scope : (string * int) list ) (funs:(string*int) list) : instruction list =
  let if_is = (emit_expr e_if si scope funs) in
  let then_is = (emit_expr e_then si scope funs) in
  let else_is = (emit_expr e_else si scope funs) in
  (* create branch names *)
  let if_true = "if_true" ^ string_of_int (get_label_num()) in
  let else_branch = "else_branch" ^ string_of_int (get_label_num()) in
  let end_of_if = "end_of_if" ^ string_of_int (get_label_num()) in
  if_is (* evaluate condition *)
  @ [ IMov(e_true, Reg(R10)) ]
  @ [ ICmp(Reg(R10), Reg(RAX)) ]
  @ [ IJEQ(if_true) ]
  @ [ IMov(e_false, Reg(R10)) ]
  @ [ ICmp(Reg(R10), Reg(RAX)) ]
  @ [ IJEQ(else_branch) ]
  (* Not a boolean, error *)
  @ [ IMov(Const(-3), Reg(RDI)) ]
  @ [ ICall("builtin_error") ]
  (* true branch *)
  @ [ ILabel(if_true) ]
  @ then_is
  @ [ IJump(end_of_if) ]
  @ [ ILabel(else_branch) ]
  (* false branch *)
  @ else_is
  @ [ ILabel(end_of_if) ]

and emit_params (params : expr list) (si : int) (scope : (string * int) list) (funs:(string*int) list) (current : int) : instruction list =
  match params with
  | [] -> [] (* done *)
  | (e)::tail -> emit_expr e si scope funs (* put current param into rax *)
                 @ save_to_stack si
                 @ emit_params tail (si+1) scope funs (current+1)

and load_params (si:int) (num:int) (current:int) : instruction list =
  if(num = current) then []
  else
    [ IMov(stackloc si, PReg("P" ^ string_of_int current)) ]
    @ load_params (si+1) num (current+1)
  
and emit_app (name : string) (e : expr list) (si : int) (scope : (string * int) list ) (funs:(string*int) list) : instruction list =
 if not (func_exists name (List.length e) funs) then (* check if func name w/ # of params exists *)
   failwith (sprintf "Error: function %s with %d params does not exist" name (List.length e))
 else
   let num_args = List.length e in
   emit_params e si scope funs 0 (* put all of 'e' onto stack *)
   @ load_params si num_args 0 (* put stack into registers *)
   @ [ ISub(Const(8 * si), Reg(RSP)) ]
   @ [ ICall(name) ]
   @ [IAdd (Const(8 * si), Reg(RSP))]


(* using a7 solution's bc it's 15 less lines than mine *)
and emit_tcreate (es : expr list) (si : int) (scope:(string*int) list) (funs:(string*int) list) : instruction list =
  let num_args = List.length es in
  let stack_args = ENumber(num_args)::es in
  (flatmapi (fun i e -> emit_expr e (si+i) scope funs
                        @ [IMov(Reg(RAX), stackloc(si+i))]) stack_args)
  @ (flatmapi (fun i _ -> [IMov(stackloc(si+i), Reg(RAX));
                           IMov(Reg(RAX), RegOffset(8*i, RBX))]) stack_args)
  @ [ IMov (Reg(RBX), Reg(RAX))
    ; IAdd (Const(8 * (num_args+1)), Reg(RBX))
    ]

and check_tuple (e : expr) (si : int) (scope : (string * int) list ) (funs:(string*int) list) : instruction list =
  let tuple_check = "tuple_check" ^ string_of_int (get_label_num()) in
  let not_tuple =  [ ISub(Const(8 * si), Reg(RSP)) ] (* not a tuple *)
                   @ [ IMov(Const(-6), Reg(RDI)) ] (* error code *)
                   @ [ ICall("builtin_error") ]
                   @ [ IAdd (Const(8 * si), Reg(RSP)) ] in
  emit_expr e si scope funs (* get address of tuple in heap *)  
  @ save_to_stack si (* save RAX *)
  @ typecheck TTup (* check if it's a tuple *)
  @ [ IJEQ(tuple_check) ] (* is a tuple, jump *)
  @ not_tuple (* call error function *)
  @ [ ILabel(tuple_check) ]
  @ restore_stack si

and emit_tget (e1: expr) (e2 : expr) (si : int) (scope : (string * int) list ) (funs:(string*int) list) : instruction list =
  let in_range =  "in_range" ^ string_of_int (get_label_num()) in
  let error = "error" ^ string_of_int (get_label_num()) in
  check_tuple e1 si scope funs (* check if tuple and save address to stack *)
  @ emit_expr e2 (si+1) scope funs (* get index *)
  @ save_to_stack (si+2) @ check_int (si+2) @ restore_stack (si+2) (* check if index is int*)
  @ [ IMov(stackloc si, Reg(R10)) ] (* put tuple's address (saved to stack in 'check_tuple') in R10 *)
  (* check upper bound *)
  @ [ ICmp(MemAddr(R10), Reg(RAX)) ]
  @ [ IJGTE(error) ]
  (* check lower bound *)
  @ [ ISar(Const(1), Reg(RAX)) ] (* shift index right *)
  @ [ ICmp(Const(0), Reg(RAX)) ]
  @ [ IJGTE(in_range) ] (* greater than 0, good *)

  @ [ ILabel(error) ]
  @ [ ISub(Const(8 * si), Reg(RSP)) ]
  @ [ IMov(Const(-7), Reg(RDI)) ] (* error code *)
  @ [ ICall("builtin_error") ]
  @ [ IAdd (Const(8 * si), Reg(RSP)) ]

  @ [ ILabel(in_range) ]
  @ [ ITupIndex(Reg(R10), Reg(RAX), Reg(RAX)) ] (* put result in rax *)

and emit_while (e1 : expr) (e2 : expr) (si : int) (scope :(string*int) list) (funs:(string*int) list) : instruction list =
  let while_start = "while_start" ^ string_of_int (get_label_num()) in
  let while_end = "while_end" ^ string_of_int (get_label_num()) in
  emit_expr e1 si scope funs
  @ [ IMov(Reg(RAX), Reg(R10)) ] 
  @ [ IAnd(type_mask TBool, Reg(R10)) ] (* check if condition is boolean *)
  @ [ ICmp(type_tag TBool, Reg(R10)) ]
  @ [ IJEQ(while_start) ]

  @ [ ISub(Const(8 * si), Reg(RSP)) ] 
  @ [ IMov(Const(-9), Reg(RDI)) ] (* error code *)
  @ [ ICall("builtin_error") ]
  @ [ IAdd (Const(8 * si), Reg(RSP)) ]
      
  @ [ ILabel(while_start) ]
  @ emit_expr e1 si scope funs (* check if condition is true *)
  @ [ ICmp(e_true, Reg(RAX)) ]
  @ [ IJNEQ(while_end) ] (* false, finish loop *)
  @ emit_expr e2 si scope funs (* body of loop *)
  @ [ IJump(while_start) ]

  @ [ ILabel(while_end) ]
  @ [ IMov(e_false, Reg(RAX)) ] (* loop evaluates to false *)

and emit_begin (es : expr list) (si : int) (scope :(string*int) list) (funs:(string*int) list) : instruction list =
  match es with
  | ex::tl -> emit_expr ex si scope funs @ emit_begin tl si scope funs
  | [] -> []

and emit_assign (e1:expr) (e2:expr) (si:int) (scope:(string*int) list) (funs:(string*int) list) : instruction list =
  let id = (match e1 with (* check if identifier *)
            | EId(id) -> (match List.assoc_opt id scope with (* check if id exists *)
                          | Some (i) -> stackloc i
                          | None -> failwith (sprintf "Unbound id: %s" id)
                         )
            | _ -> failwith (sprintf "Assignment must be identifier")
           ) in
  emit_expr e2 si scope funs
  @ [ IMov(Reg(RAX), id) ]


(* structurally equivalent *)
and emit_sequiv (e1:expr) (e2:expr) (si:int) (scope:(string*int) list) (funs:(string*int) list) : instruction list =
  let not_tuple = "not_tuple" ^ string_of_int (get_label_num()) in
  let tuple_check = "tuple_check" ^ string_of_int (get_label_num()) in
  let end_sequiv = "end_sequiv" ^ string_of_int (get_label_num()) in
  let e1_is = (emit_expr e1 si scope funs) @ save_to_stack si in
  let e2_is = (emit_expr e2 (si+1) scope funs) @ save_to_stack (si+1) in
  e1_is
  @ [ IAnd(type_mask TTup, Reg(RAX)) ] (* check if e1 tuple *)
  @ [ ICmp(type_tag TTup, Reg(RAX)) ]
  @ [ IJNEQ(not_tuple) ]
  @ e2_is
  @ [ IAnd(type_mask TTup, Reg(RAX)) ] (* check if e2 tuple *)
  @ [ ICmp(type_tag TTup, Reg(RAX)) ]
  @ [ IJEQ(tuple_check) ]

  @ [ ISub(Const(8 * si), Reg(RSP)) ] (* e1 or e2 is tuple, other is not *)
  @ [ IMov(Const(-10), Reg(RDI)) ] (* error code *)
  @ [ ICall("builtin_error") ]
  @ [ IAdd (Const(8 * si), Reg(RSP)) ]

  @ [ ILabel(tuple_check) ] (* do tuple comparison *)
  @ [ IMov(stackloc si, Reg(RDI)) ] 
  @ [ IMov(stackloc (si+1), Reg(RSI)) ]
  @ [ ISub(Const(8 * si), Reg(RSP)) ] 
  @ [ ICall("tuple_comparison") ]
  @ [ IAdd(Const(8 * si), Reg(RSP)) ]
  @ [ IJump(end_sequiv) ]

  @ [ ILabel(not_tuple) ] (* not a tuple, do normal comparison *)
  @ emit_binop Equivalent e1 e2 si scope funs
  @ [ ILabel(end_sequiv) ]


and emit_expr (e : expr) (si : int) (scope : (string * int) list) (funs:(string*int) list) : instruction list =
  match e with
  | ENumber(n) -> [IMov (Const( (n lsl 1) lor 1 ), Reg(RAX))]
  | EBool(b) -> let bool = (match b with
                         | True -> e_true
                         | False -> e_false) in
                [IMov(bool, Reg(RAX)) ] (* move T/F constant into RAX *)
  | EId(id) -> let arg = (match List.assoc_opt id scope with
                          | Some (i) -> stackloc i
                          | None -> failwith (sprintf "Unbound id: %s" id)
                         )
               in [ IMov(arg, Reg(RAX)) ]
  | EBinop(op, e1, e2) -> emit_binop op e1 e2 si scope funs
  | ELet(bs, body) -> (match (find_dup bs) with
                       | Some(d) -> failwith (sprintf "Duplicate binding: %s" d)
                       | None -> emit_let bs body si scope funs
                      )
  | EIf(e1, e2, e3) -> emit_if e1 e2 e3 si scope funs
  | EUnop(op, e) -> emit_unop op e si scope funs
  | EApp(name, e) -> emit_app name e si scope funs
  | ETup(es) -> emit_tcreate es si scope funs
  | ETLen(e) -> check_tuple e si scope funs @ [ IMov(MemAddr(RAX), Reg(RAX)) ]
  | ETIs(e) ->  emit_expr e si scope funs @ is_type TTup
  | ETGet(e1, e2) -> emit_tget e1 e2 si scope funs
  | EWhile(e1, e2) -> emit_while e1 e2 si scope funs
  | EAssign(e1, e2) -> emit_assign e1 e2 si scope funs
  | EBegin(es) -> emit_begin es si scope funs
  | ESEquiv(e1, e2) -> emit_sequiv e1 e2 si scope funs

(* ------------------------------------------------------------------------ *)
let find_func_dup (bs : (string * int) list) =
  let rec find_func_helper lst b =
    match lst with
    | [] -> None
    | (name,_)::tail -> if (List.exists (fun x -> x = name) b)
                            then Some(name)
                            else find_func_helper tail (name::b)
  in
  find_func_helper bs []
let func_param_check (params : string list) =
  let rec find_param_helper lst b =
    match lst with
    | [] -> None
    | (name)::tail -> if (List.exists (fun x -> x = name) b)
                            then Some(name)
                            else find_param_helper tail (name::b)
  in
  find_param_helper params []  


(* funs (string * int) list -> contains all functions. string=name, int=num of params*)
let rec emit_prog (d : definition list) (si : int) (scope : (string*int) list) (funs:(string*int) list) : instruction list =
  (match d with
   | DFun(name,param,exp)::tail ->
      if name = "main" then (* main function, check that it's correct *)
        if ((List.length param = 1) && (List.hd param = "input")) then (* correct definition *)
          if List.length tail > 0 then (* something is after main *)
            failwith "Error: Invalid syntax. main function must be last"
          else (match find_func_dup funs with (* check for dups *)
                | Some(d) -> failwith (sprintf "Duplicate function names: %s" d)
                | None -> [ ILabel("code_entry_point") ]
                          @ [ IComm("put input on stack") ]
                          @ [ IMov(Reg(RDI), RegOffset(-8, RSP)) ] (* put input on stack *)
                          @ [ IComm("store heap pointer in rbx") ]
                          @ [ IMov(Reg(RSI), Reg(RBX)) ]  (* heap location *)
                          @ [ IAdd(Const(8), Reg(RBX)) ]
                          @ [ IAnd(Const(-4), Reg(RBX)) ] (* give tuple tag *)
                          @ [ IComm("begin program") ]
                          @ emit_expr exp si scope funs (* main body *)
          )
        else failwith "Error: main function requires 'input' as parameter"
   
      else if List.length param > 6 then
        failwith (sprintf "Error: Cannot have more than 6 parameters in function %s" name)

      else (* user defined function *)
        (match func_param_check param with
         | Some(s) -> failwith (sprintf "Error: Duplicate identifiers in function %s" name)
            
         | None -> (* user defined function *)
            [ ILabel(name) ]
            @ func_params param exp si [] funs 0
            @ [ IRet ]
            @ (emit_prog tail si scope funs)) (* continue *)
  
   | [] -> failwith "Error: No main function with 'input' param found" (* reached end without finding main*) 
  )      

and func_params (params : string list) (body : expr) (si : int) (scope : (string*int) list) (funs:(string*int) list) (current:int) : instruction list =
  match params with
  | [] -> emit_expr body si scope funs
  | (p)::tail -> [ IMov(PReg("P" ^ string_of_int current), Reg(RAX)) ]
                 @ save_to_stack si
                 @ func_params tail body (si+1) ((p,si)::scope) funs (current+1)


let rec get_func_names (d : definition list) (funs:(string*int) list) : (string * int) list =
  match d with
  | [] -> funs
  | DFun(name,param,exp)::tail -> 
     if name = "main" then funs
     else get_func_names tail ((name,(List.length param))::funs)


let codegen prog =
  let expr = (match prog with
              | Program(e) -> e
             ) 
  in
  let prelude =".text
  .globl print
  .globl builtin_error
  .globl code_entry_point
"
(*
code_entry_point:
  movq %rdi, -8(%rsp)
 *)
  in
  let func_names = (get_func_names expr []) in
  let compiled = (emit_prog expr 2 [("input", 1)]) func_names in
  let assembly_string = (to_asm (compiled @ [IRet])) in
  sprintf "%s%s\n" prelude assembly_string


