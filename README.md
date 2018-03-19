# Compilers-Course
Code made for my Compiler's course

HW 1: 
1. Write a pretty printer for depython programs pretty : prog -> unit that prints out a human readable version of the given program.

2. Write an interpreter for depython, following the instructions for the SLP interpreter. You will need a function interp : prog -> unit that interprets the top level program, and auxiliary functions to interpret the stm and exp datatypes in an appropriate environment. You should have mutualy recursive functions interpStm stm * table -> table and interpExp exp * table -> int * table. A table is a list of (id, int) pairs, and the first id that matches should hold the current value of the id. You should have update and lookup functions that add new bindings and lookup current values. update : table * id * int -> table takes a table, and adds a binding of id to the given int. lookup : table * id -> int looks up the id in the given table and returns the current value.
