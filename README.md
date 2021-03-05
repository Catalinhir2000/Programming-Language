Design an original programming language and provide a syntactic analyzer for it, using YACC.
Your language should include
● predefined types: int, float, char, string, bool (you can use your own syntax for the values
of the char, string, and bool types)
● user defined data types (similar to classes in object orientated languages, but with your
own syntax); provide specific syntax to allow initialization and use of variables of these
types
● array types
● variable declarations/definition, constant definitions
● control statements (if, for, while, etc.), assignment statements
● function declarations
● arithmetic and boolean expressions
● operations with string types
● function calls which can have as parameters: expressions, other function calls,
identifiers, constants, etc.
● A predefined function called Eval which has a parameter of type int
Your yacc analyzer should:
1) do the syntactic analysis of the program (3p)
2) create a symbol table for every input source program in your language, which should include:
- information regarding variable or constant identifiers (type, value – for identifiers
having a predefined type, scope: global definition, defined inside a function, defined
inside a user defined type, or any other situation specific to your language) (1p)
- information regarding function identifiers (the function signature, whether the function
is a method in a class etc) (1p)
 The symbol table should be printable in a file (symbol_table.txt)
3) provide semantic analysis for the following:
- any variable that appears in a program has been previously defined (0.25p)
- a variable should not be declared more than once (0.25p)
- a variable appearing in the right side of an expression should have been initialized
explicitly (0.25p)
- a function is not defined more than once with the same signature (0.25p)
- a function that is called in the program has been defined (0.25p)
- the left side of an assignment has the same type as the right side (0.25p)
- the parameters of a function call have the types from the function definition (0.25p)
- in any call to the Eval, the parameter has type int (0.25p)
- accessing elements of arrays (0.5p)
- run if and while statements (1p)
- run user defined functions (1p)
Error messages should be provided if these conditions do not hold;
4) provide the evaluation of integer arithmetic expressions in a program ; if a program is
syntactically and semantically correct and the expr type is int , for every call of the form 
Eval(expr) , the actual value of expr will be printed. If the program is not syntactically correct the
eval values should not be printed (0.5p)
Besides the homework presentation, students should be able to answer specific questions
regarding grammars and parsing algorithms (related to the first part of your homework) or yacc
details related to the second part (the answers will also be graded).
