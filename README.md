# OPythn
OPythn is a compiler and bytecode interpreter for a core subset of Python. The language specification and implementation details can be found [here](./docs/specification.pdf).

### Installation

To be able to compile OPythn, you must have [OCaml](http://caml.inria.fr/download.en.html) installed on your system, as well as the following packages:

+ [extlib](https://github.com/ygrek/ocaml-extlib)
+ [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving)

If you use OPAM, running `opam install extlib` and `opam install ppx_deriving` should work.  

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

### Credits
Created by Marcel Goh under the supervision of Adam Dingle at the Faculty of Mathematics and Physics, Charles University in Prague.
