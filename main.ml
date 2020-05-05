open Printf

let () =
    let name = Sys.argv.(1) in
    let sexpr = SExpr.parse_file name in
    let ast = Parser.sexpr_list_to_ast sexpr in
(*
    Ast.print_ast ast;
 *)
    let program = Codegen.codegen ast in
    printf "%s\n" program

