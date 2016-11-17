# OCaml-Mini-Interpreter
A basic interpreter written in [OCaml](http://www.ocaml.org/) for a subset of the OCaml language, supportive of arithmetic operations, function declarations, recursions and the list data structure. This interpreter makes use of [OCamllex](http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html#toc105) and [OCamlyacc](http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html#toc107) to generate a lexer and parser. It interprets a subset of OCaml (details listed below) into OCaml language. To run it the OCaml compiler should be installed. First type `make` and then type `./nanoml.top` to open the interpreter shell. Press CTRL+D to exit the shell. Or use `./nanoml.byte filename.ml` to run your own script. An example codes is in `test.ml`, which declares a function to compute the Fibonacci sequence. 

The subset of OCaml supports all the basic arithmatic operations as well as local binding, function declaration in form of local binding, operation precedence, recursive function declaration in form of local binding, and the list data structure in the offical OCaml language. 
