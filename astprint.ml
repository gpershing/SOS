(* Very basic """pretty""" printer for the AST *)
(* Written by G *)
open Ast

let rec comma_list_str f l = match l with
  [] -> ""
| hd :: tl -> match tl with
    [] -> f hd
  | _ -> f hd ^ ", " ^ comma_list_str f tl

let rec typeid_str t = match t with
  TypeID(s) -> s
| ArrayTypeID(p) -> "array " ^ typeid_str p
| FxnTypeID(l, t) -> "func " ^ comma_list_str typeid_str l ^" -> "^typeid_str t

let basic_print prog = 
  let rec print_stmt = function
    Typedef(t) -> let print_tdef = function
      Alias(a, b) -> print_endline ("alias " ^ a ^ " " ^ typeid_str(b))
    | StructDef(a, b) -> print_endline ("struct " ^ a ^ " = {" ^ comma_list_str (fun (a, b) -> typeid_str(a) ^ " " ^ b) b ^ "}")
    in print_tdef t
  | FxnDef(a, b, c, d) -> print_endline (typeid_str(a) ^ " " ^ b ^ "(" ^ comma_list_str (fun (a, b) -> typeid_str(a) ^ " " ^ b) c ^ ") = "); print_stmt (Expression(d))
  | Expression(e) -> let rec expr_str = function
      VarDef(a, b, c) -> typeid_str(a) ^ " " ^ b ^ " = " ^ expr_str c
    | Assign(a, b) -> a ^ " = " ^ expr_str b
    | AssignStruct(a, b, c) -> expr_str a ^ "." ^ b ^ " = " ^ expr_str c
    | AssignArray(a, b, c) -> expr_str a^"["^expr_str b ^"] = "^ expr_str c
    | ArrayAccess(nm, idx) -> expr_str nm^"["^expr_str idx^"]"
    | Uop(a, b) -> let uoperator_str = function Not -> "!" | Neg -> "-" in uoperator_str a ^ expr_str b
    | Binop(a, b, c) -> let operator_str = function
        Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | MMul -> "**"
      | Div -> "/"
      | Mod -> "%"
      | Eq -> "="
      | Neq -> "!="
      | Less -> "<"
      | Greater -> ">"
      | LessEq -> "<="
      | GreaterEq -> ">="
      | And -> "&&"
      | Or -> "||"
      | Of -> "of"
      | Concat -> "@"
      | Seq -> ";" in
      "(" ^ expr_str a ^ " " ^ operator_str b ^ " " ^ expr_str c ^ ")"
    | FxnApp(a, b) -> 
        expr_str a ^ "(" ^ comma_list_str expr_str b ^ ")"
    | IfElse(a, b, c) -> "if (" ^ expr_str a ^ ")\n then (" ^ expr_str b ^ ")\n else (" ^ expr_str c ^")\n"
    | ArrayCon(a) -> "[" ^ comma_list_str expr_str a ^ "]"
    | AnonStruct(a) -> "{" ^ comma_list_str expr_str a ^ "}"
    | NamedStruct(a, b) -> a ^ "{" ^ comma_list_str expr_str b ^ "}"
    | Var(a) -> a
    | StructField(a, b) -> expr_str a ^ "." ^ b
    | IntLit(i) -> string_of_int i
    | FloatLit(f) -> f
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    in print_endline(expr_str e)
  in
  List.iter print_stmt prog

(*let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Scanner.token lexbuf in
  basic_print prog *)
