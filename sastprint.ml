open Sast

let basic_print sast = 
  let rec sargl_string l =
    List.fold_left (fun str (t, id) -> (if str = "" then "" else str^", ")^typeid_string t ^ id) "" l
  and typeid_string = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"
  | Array(t) -> "array "^typeid_string t
  | Struct(l) -> "{"^sargl_string l^"}"
  | EmptyArray -> "[]"
  in

  let print_typedef = function
    SAlias(nm, al) -> print_string("alias "^nm^" = "^typeid_string al^"\n")
  | SStructDef(nm,  l) -> print_string("struct "^nm^" = {"^sargl_string l^"}\n")
  in

  let print_sstmt = function
    STypeDef(td) -> print_typedef td
  | SExpression(sexpr) -> print_string("")
  | SImport(imp) -> print_string("import "^imp^"\n")

  in List.iter print_sstmt sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
  basic_print sast
