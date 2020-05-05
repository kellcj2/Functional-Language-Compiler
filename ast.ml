(*
Modified By: Christopher J. Kelly
Due Date:    4/8/2020
Assignment:  #6
Course:      Compilers II
Professor:   Schwesinger
Filename:    ast.ml
Purpose:     Generates an ast for the language
*)

type unop =
  | Print
  | IsBool
  | IsInt

type binop =
  | Plus
  | Minus
  | Mult
  | LessThan
  | GreaterThan
  | Equivalent

type boolean =
  | True
  | False

type expr =
  | ENumber of int
  | EBinop of binop * expr * expr
  | ELet of (string * expr) list * expr
  | EId of string
  | EIf of expr * expr * expr 
  | EBool of boolean
  | EApp of string * expr list
  | EUnop of unop * expr
  | ETup of expr list
  | ETLen of expr
  | ETGet of expr * expr
  | ETIs of expr
  | EWhile of expr * expr
  | EAssign of expr * expr
  | EBegin of expr list
  | ESEquiv of expr * expr

type definition =
  | DFun of string * string list * expr

type program =
  | Program of definition list

(* printing stuff *)
let string_of_binop b =
  match b with
  | Plus -> "(Plus) "
  | Minus -> "(Minus) "
  | Mult -> "(Mult) "
  | LessThan -> "(LessThan) "
  | GreaterThan -> "(GreaterThan) "
  | Equivalent -> "(Equivalent) "

let string_of_bool b =
  match b with
  | True -> "(True) "
  | False -> "(False) "

let rec string_of_exp ast =
  match ast with
    | ENumber(n) -> "(ENumber " ^ (string_of_int n) ^ ") "
    | EBinop(op, e1, e2) -> "(EBinop " ^ (string_of_binop op) ^ (string_of_exp e1) ^ (string_of_exp e2) ^ ") "
    | EId(id) -> "(EId " ^ (id) ^ ") "
    | ELet(xs,e) -> "(ELet " ^ (bindings_to_string xs) ^ (string_of_exp e) ^ ") "
    | EIf(e_if, e_then, e_else) -> "(EIf " ^ (string_of_exp e_if) ^ " Then " ^ (string_of_exp e_then) ^ " Else " ^ (string_of_exp e_else) ^ ") "
    | EBool(b) -> "(EBool " ^ (string_of_bool b) ^ ") "
    | EApp(id, es) -> "(EApp " ^ id ^ " " ^ (string_of_exprs es) ^ ") "
    | EUnop(u, e) -> "(EUnop " ^ (unop_to_string u) ^ " " ^ (string_of_exp e) ^ ") "
    | ETup(es) -> "(ETup " ^ (string_of_exprs es) ^ ") "
    | ETLen(e) -> "(ETLen " ^ (string_of_exp e) ^ ") "
    | ETGet(e1,e2) -> "(ETGet " ^ (string_of_exp e1) ^ " (Index " ^ (string_of_exp e2) ^ ") ) "
    | ETIs(e) -> "(ETIs " ^ (string_of_exp e) ^ ") "
    | EWhile(e1, e2) -> "(EWhile " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ") "
    | EAssign(e1, e2) -> "(EAssign " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ") "
    | EBegin(es) -> "(EBegin " ^ (string_of_exprs es) ^ ") "
    | ESEquiv(e1, e2) -> "(ESEquiv " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ") "

and unop_to_string u =
  match u with
  | Print  -> "(Print)"
  | IsBool -> "(IsBool)"
  | IsInt  -> "(IsInt)"
    
and bindings_to_string bs =
  match bs with
  | [] -> ""
  | (i,e)::xs -> "(" ^ i ^ " " ^ (string_of_exp e) ^ ") " ^ (bindings_to_string xs)

and string_of_exprs es =
  match es with
  | [] -> ""
  | h::t -> (string_of_exp h) ^ (string_of_exprs t)

let rec string_of_params ps =
  match ps with
  | [] -> ""
  | h::t -> "(Param " ^ h ^ ") " ^ string_of_params(t)

let string_of_def d =
  match d with
  | DFun(id,params,body) -> "(DFun " ^ id ^ " ( " ^ (string_of_params params) ^ ") " ^ (string_of_exp body) ^ ") "

let rec string_of_defs ds =
  match ds with
  | [] -> ""
  | h::t -> (string_of_def h) ^ (string_of_defs t)

let string_of_prog ast =
  match ast with
  | Program(e) -> "(Program " ^ (string_of_defs e) ^ ")\n"

let print_ast ast = print_endline(string_of_prog ast)
            
