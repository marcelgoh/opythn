# OPythn
OPythn is a compiler and bytecode interpreter for a core subset of Python. The language specification and implementation details can be found [here](./docs/specification.pdf).

### Installation

To be able to compile OPythn, you must have [OCaml](http://caml.inria.fr/download.en.html) installed on your system, as well as the following packages:

+ [extlib](https://github.com/ygrek/ocaml-extlib)
+ [menhir](http://gallium.inria.fr/~fpottier/menhir/)
+ [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving)

If you use OPAM, running `opam install extlib menhir ppx_deriving` should get you set up.  

Once you have all the requisite packages installed, simply run `make` from the main `opythn` directory to compile the program. This should not take more than a second or two. To delete compiled output, run `make clean`.

### Usage

From the main `opythn` directory, running `./main` with no program arguments starts the OPythn read-eval-print loop (REPL). Enter statements line-by-line for OPythn to evaluate. The REPL saves the current environment between inputs.

```
+----------------------------------------------+
|            OPYTHN INTERACTIVE MODE           |
|   Author: Marcel Goh (Release: 21.04.2019)   |
|            Type "Ctrl-C" to quit.            |
+----------------------------------------------+
]=>
```

Alternatively, you can write OPythn source code in your favourite text editor and save it in a `.opy` file. Then simply run `./main [filename]` to run your code. You can try this out with some of the programs in the `examples` folder (e.g. `./main examples/is_it_ok.opy`).

### Running tests

The `test` folder contains a bunch of test cases. If you plan on modifying the interpreter in any way, you should periodically run the tester to ensure that everything still works: From the main `opythn` directory, run `cd test` and then `sh run_tests.sh` to run all tests. If all goes well you'll see a message that says `All tests passed.` If not, run `cat diff_log.txt` to see which cases failed. You can add a new test case by simply adding a `.opy` file to the `test` folder. The tester runs each file with Python 3 and OPythn and then compares the outputs. (You'll need Python 3 installed on your system for this to work.)

### Credits
Created by Marcel Goh under the supervision of Adam Dingle at the Faculty of Mathematics and Physics, Charles University in Prague.
