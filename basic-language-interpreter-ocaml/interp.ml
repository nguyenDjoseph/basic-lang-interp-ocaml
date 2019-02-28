(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)
open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

(*----- BINARY OPERATOR -----*)
let binOp (op : Absyn.oper) val1 val2 : float =
    Hashtbl.find Tables.binary_fn_table op val1 val2;;
(*----- UNARY OPERATOR -----*)
let unaryOp (op : Absyn.oper) val1 : float =
    Hashtbl.find Tables.unary_fn_table op val1;;
(*----- COMPARISON OP FOR IF STATEMENT ------*)
let comparisonOp (op : Absyn.oper) val1 val2 : bool =
    Hashtbl.find Tables.comparison_fn_tbl op val1 val2;;
(* -------------------------------- *)
let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with
        | Arrayref (ident, expr) ->
            let idx = eval_expr(expr) in
            let getArray = Hashtbl.find Tables.array_table ident in
                getArray.(int_of_float(idx) - 1)
        | Variable ident -> Hashtbl.find Tables.variable_table ident)
    | Unary (oper, expr) ->
        let calculatedExpr = eval_expr(expr) in
            unaryOp oper calculatedExpr
    | Binary (oper, expr1, expr2) ->
        let v1 = eval_expr(expr1) in
        let v2 = eval_expr(expr2) in
            binOp oper v1 v2
(* ------ IF Statement Helper Fn -----*)
let rec eval_if (expr : Absyn.expr) : bool = match expr with
    | Binary (oper, expr1, expr2) ->
        let v1 = eval_expr(expr1) in
        let v2 = eval_expr(expr2) in
            comparisonOp oper v1 v2
    | _ -> false
(*------- INTERPRET FUNCTIONS -------*)
let interp_let (mem_ref : Absyn.memref) val1 = match mem_ref with
    | Arrayref (ident, expr) ->
        let idx = eval_expr(expr) in
        let getArray = Hashtbl.find Tables.array_table ident in
            getArray.(int_of_float(idx) - 1) <- val1
    | Variable ident -> Hashtbl.add Tables.variable_table ident val1
let interp_dim ident val1 =
    let newArr = Array.make val1 0.0 in
        Hashtbl.add Tables.array_table ident newArr
(* ------- PLACE HOLDER FNS -------*)
(* Goto statement is completed inside the interpret function *)
let interp_goto labsl = print_string ""
(* If statement is completed inside the interpret function *)
let interp_if expr label = print_string ""
(* --------------------------------- *)

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        try  let number = Etc.read_number ()
             in if number == nan then 
                Etc.die ["Bad input: expected a number"];
                interp_let memref number
        with End_of_file ->
             Hashtbl.replace Tables.variable_table "eof" 1.0
             (*in interp_let memref number;*)
    in List.iter input_number memref_list

let interp_stmt (stmt : Absyn.stmt) = match stmt with
    | Dim (ident, expr) ->
        let val1 = eval_expr(expr) in
            interp_dim ident (int_of_float val1);
    | Let (memref, expr) ->
        let val1 = eval_expr(expr) in
            interp_let memref val1
    | Goto labsl -> interp_goto labsl
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list
    | Input memref_list -> interp_input memref_list

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt ->  match stmt with
        | Goto labsl ->
            let goto_program = Hashtbl.find Tables.label_table labsl in
                interpret goto_program;
        | If (expr, label) ->
            let if_taken = if eval_if expr
                then Hashtbl.find Tables.label_table label
                else otherlines
                in interpret if_taken
        | _ -> interp_stmt stmt; interpret otherlines

let interpret_program program =
    (Tables.init_label_table program;
     (*Dumper.dump_program(Hashtbl.find Tables.label_table "zero");*)
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
