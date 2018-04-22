(*
 	Lillian Gonzalez Albino
	Compilers Course
 *)

(*
	For convention, parameters are written as stm#, exp#, num#, name#, val#, tab#
	representing statements, expressions, numbers, names, values, and tables respectively
*)

(* load depython.sml*)
use "depython.sml";

(* 	
	input: BinOp (Add, Mult) 
	output: unit
	Prints the correct operator according to the binary operation given.
*)
fun prettyBinop Add = print " + "
	| prettyBinop Mult = print " * "

(*
	input: exp (Num, BinOp, Name)
	output: unit
	Manages expressions. Prints Nums and Names, and it calls 
	itself recursively to manage binary operations. Uses prettyBinop.
*)
fun prettyExp (Num num1) = print(Int.toString num1)
	| prettyExp (BinOp (exp1, op1, exp2)) = (prettyExp exp1 ; prettyBinop op1 ; prettyExp exp2)
	| prettyExp (Name name1) = print(name1)

(*
	input: stm (PrintStm, Expr, CompoundStm, Assign)
	output: unit
	Manages statements. Uses prettyExp to manage the expressions and
	prints the parts of the statements that prettyExp does not, 
	i.g. "print", "\n", "=".
*)
fun prettyStm (PrintStm exp1) = (print "print " ; prettyExp exp1 ; print "\n")
	| prettyStm (Expr exp1) = (prettyExp exp1 ; print "\n")
	| prettyStm (CompoundStm (stm1, stm2)) = (prettyStm stm1 ; prettyStm stm2)
	| prettyStm (Assign (id1, exp1)) = (prettyExp (Name id1) ; print " = " ; prettyExp exp1 ; print "\n")

(*
	input: prog (Module)
	output: unit
	Prints the program by calling prettyStm to manage statements.
*)
fun pretty (Module stm1) = prettyStm stm1 

(* 
	give an alias of a table as a list of tuples of an id and an int 
	table : list of id*int 
*)
type table = (id * int) list

(*
	input: table, id, int
	output: table
	Cons a tuple of the given id and num to the given table.
*)
fun update (tab1:table, id1:id, num1:int) = (id1, num1)::tab1 

(*
	input: table, id
	output: int
	If the table is empty, it raises an exception since the given id is 
	not in the table. Otherwise, it checks the first element of the table, if the
	given id and the id of the tuple match, it returns the int value 
	of the tuple. If the ids do not match, it calls itself recursively to 
	continue checking the rest of the table for a matching id value to the one given. 
*) 
fun lookup ([]:table, id1:id) = raise (Fail "Unbounded identifier")
	| lookup( (id1, val1)::tail , name1) = if id1=name1 then val1 else lookup(tail, name1)

(*
	input: exp (Num, Name, BinOp)
	output: int*table
	Manages the interpretation of the expressions. For Num and Name it 
	evaluates the number and the name respectively and it returns the tuple. 
	For BinOp, it interprets the RHS and the LHS of the expression
	and then it evaluates the complete expression according to the binary 
	operator.
*) 
fun interpExp (Num num, tab1) = (num, tab1)
	| interpExp (Name id, tab1) = (lookup(tab1, id), tab1)
	| interpExp (BinOp (exp1, op1, exp2), tab1) = 
		let
			val lhs = interpExp(exp1, tab1)
			val rhs = interpExp(exp2, tab1)
		in
			if op1=Add then ((#1 lhs) + (#1 rhs), tab1) else ((#1 lhs) * (#1 rhs), tab1)
		end

(*
	input: stm (Expr, PrintStm, CompoundStm, Assign)
	output: table
	Manages statements. Uses interpExp. 
	To be commented by cases.
*)
(*
	Expr:
	Evaluates the expression using interpExp and then returns the same table.
*) 
fun interpStm (Expr exp1, tab1) = 
		let
			val val1 = interpExp(exp1, tab1)
		in
			tab1
		end
(*
	PrintStm:
	Evaluates the expression using interpExp, casts the first element 
	of the tuple interpExp returns (an int) to a string 
	to be printed and then returns the table from interpExp. 
*)
	| interpStm (PrintStm exp1, tab1) = 
		let
			val (val1, tab2) = interpExp(exp1, tab1)
			val val2 = Int.toString(val1)
		in
			(print val2 ; print "\n" ; tab2 )
		end
(*
	CompoundStm:
	Calls itself recursively to evaluate both statements linearly. After 
	evaluating the first statement, it uses the table returned from the first 
	statement as the second argument for the evaluation of the second statement.
	Returns the table.
*)
	| interpStm (CompoundStm (stm1, stm2), tab1) = 
		let
			val tab2 = interpStm(stm1, tab1)
			val tab3 = interpStm(stm2, tab2)
		in
			tab3
		end
(*
	Assign:
	Evaluates the expression using interpExp, then it takes the value and 
	the table that interpExp returned and uses the function update to cons 
	them as a tuple to the given table. 
	Note: this is the only case in interpStm that changes the table by
	cons-ing a new tuple to it.
*) 
	| interpStm (Assign (id1:id, exp1), tab1) = 
		let
			val (val1, tab2) = interpExp(exp1, tab1)
		in
			update(tab2, id1, val1)
		end

(*
	input: prog (Module)
	output: unit
	Manages the program by calling interpStm with an empty table. After evaluating 
	interpStm, it evaluates () to return unit.
*)
fun interp (Module stm) = (interpStm(stm, [] :table) ; ())

