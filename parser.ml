(*
Modified By: Christopher J. Kelly
Due Date:    4/8/2020
Assignment:  #6
Course:      Compilers II
Professor:   Schwesinger
Filename:    parser.ml
Purpose:     Parses input for asm generation
*)

open Printf

let int_of_string_opt s =
  try Some(int_of_string s) with
  | _ -> None

let primitive_of_string (a : string) : Ast.expr =
  if a = "true" then EBool(True)
  else if a = "false" then EBool(False)
  else
    (match int_of_string_opt a with
     | Some(x) -> ENumber(x)
     | None -> EId(a)
    )

let rec sexpr_to_expr (s : SExpr.sexpr) : Ast.expr =
  match s with
    | (Atom tag) -> primitive_of_string tag
    | (Expr e) ->
       (match e with
         | Atom("let")::(Expr bs)::exp::[] -> ELet( (binding_to_ast bs), (sexpr_to_expr exp) )
         | Atom("add1")::e1::[]  -> EBinop(Plus, sexpr_to_expr e1, ENumber(1))
         | Atom("sub1")::e1::[]  -> EBinop(Minus, sexpr_to_expr e1, ENumber(1))
         | Atom("+")::e1::e2::[] -> EBinop(Plus, sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("-")::e1::e2::[] -> EBinop(Minus, sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("*")::e1::e2::[] -> EBinop(Mult, sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("<")::e1::e2::[] -> EBinop(LessThan, sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom(">")::e1::e2::[] -> EBinop(GreaterThan, sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("==")::e1::e2::[] -> EBinop(Equivalent, sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("if")::e_if::e_then::e_else::[] -> EIf(sexpr_to_expr e_if, sexpr_to_expr e_then, sexpr_to_expr e_else)
         | Atom("print")::e1::[]   -> EUnop(Print, sexpr_to_expr e1)
         | Atom("is-int")::e1::[]  -> EUnop(IsInt, sexpr_to_expr e1)
         | Atom("is-bool")::e1::[] -> EUnop(IsBool, sexpr_to_expr e1)
         | Atom("tup")::elist      -> ETup(sexprs_to_exprs elist)
         | Atom("tup-len")::e1::[] -> ETLen(sexpr_to_expr e1)
         | Atom("tup-get")::e1::e2::[] -> ETGet(sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("is-tup")::e1::[]  -> ETIs(sexpr_to_expr e1)
         | Atom("while")::e1::e2::[] -> EWhile(sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom(":=")::e1::e2::[]  -> EAssign(sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom("begin")::elist    -> EBegin(sexprs_to_exprs elist)
         | Atom("=")::e1::e2::[]   -> ESEquiv(sexpr_to_expr e1, sexpr_to_expr e2)
         | Atom tag::rest ->
            (match primitive_of_string tag with
             | EId(id) -> EApp(tag, sexprs_to_exprs rest)
             | _ -> failwith "Invalid function name"              
            )
         | _ -> failwith "Unknown expression"
       )

and binding_to_ast (s : SExpr.sexpr list) : (string * Ast.expr) list =
  match s with
  | [] -> []
  | Expr(Atom a::value::[])::tail ->
     (match primitive_of_string a with
      | EId id -> (id, (sexpr_to_expr value)) :: (binding_to_ast tail)
      | _ -> failwith (sprintf "Invalid let binding: %s" a)
     )
  | _ -> failwith "Invalid let expression"

and  sexprs_to_exprs (s : SExpr.sexpr list) : Ast.expr list =
  match s with
  | [] -> []
  | h::t -> (sexpr_to_expr h)::(sexprs_to_exprs t)

let rec sexpr_to_param_list (ps : SExpr.sexpr list) : string list =
  match ps with
  | [] -> []
  | (Atom tag)::rest ->
      (match primitive_of_string tag with
       | EId id -> id::(sexpr_to_param_list rest)
       | _ -> failwith (sprintf "Invalid parameter: %s" tag)
      )
  | _ -> failwith "Invalid parameter list"

let sexpr_to_def (s : SExpr.sexpr) : Ast.definition =
  match s with
  | (Expr e) ->
    (match e with
    | Atom("define")::Atom a::Expr params::body::[] ->
       (match primitive_of_string a with
        | EId id -> DFun(id, sexpr_to_param_list params, sexpr_to_expr body)
        | _ -> failwith (sprintf "Invalid function name: %s" a)
       )
    | _ -> failwith "Invalid program"
    )
  | _ -> failwith "Invalid program"

let rec sexpr_list_to_def_list (ss : SExpr.sexpr list) : Ast.definition list =
  match ss with
  | [] -> []
  | h::t -> (sexpr_to_def h)::(sexpr_list_to_def_list t)


let rec sexpr_list_to_ast (ss : SExpr.sexpr list) : Ast.program =
  match List.hd ss with
    | Expr(e) ->
        ( match e with
            | Expr(e')::tl -> Program(sexpr_list_to_def_list e)
            | _ -> failwith "Invalid program"
        )
    | _ -> failwith "Invalid program"

