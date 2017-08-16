# SPL (Set Programming Language)
A simple programming language written in OCaml with the use of ocamlyacc and ocamllex. The language can be used to efficiently perform basic set operations such as intersection, union, concatenation and Kleene star.

## Requirements
* `make` command which can be installed by executing
`sudo apt-get install make` on terminal
* Source code
* A `.depend` file (even if empty) needs to be present in the folder containing all the source code.

## Compile
Compile the interpreter by following the steps below in the directory containing the source code:
1. `make clean`
2. `make depend`
3. `make`

## Run
To run the interpreter excute the following steps on terminal in the directory of the files:
`./mysplinterpreter prog.spl < input.txt` where
* `prog.spl` is your programme written with corrent spl syntax given in the user manual
* `input.txt` is the input data to the programme
