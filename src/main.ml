(* OPythn main front-end *)

open Printf

exception File_error of string

(* command-line flags *)
let debug = ref false         (* print intermediary outputs *)
let lex = ref false           (* lex-only mode *)
let load = ref ""             (* load code into REPL from files *)
let filename = ref None       (* filename *)

let set_file_to_load str = load := str
let do_nothing () = ()
(* only the first anonymous argument is saved as filename *)
let set_filename str =
  match !filename with
    None -> filename := Some str
  | Some _ -> ()

let usage_msg = "Available options:"

let speclist = [
  ("-debug", Arg.Set debug, "\tEnables printing of all intermediary representations");
  ("-lex", Arg.Set lex, "\tStarts REPL in lex-only mode");
  ("-load", Arg.String set_file_to_load, "\tStarts REPL with a file loaded");
]

(* pointer to global environment *)
let envr = ref (Interpreter.init_env ())

(* opens the specified file and outputs its contents as a string *)
let str_from_filename fname =
  try
    let ch = open_in fname in
    let size = in_channel_length ch in
    let str = really_input_string ch size in
    close_in ch;
    Some str
  with e -> None

(* print tokens generated by lexer *)
let print_lex buffer =
  let tok = ref (Lexer.read buffer) in
  while !tok <> EOF do
    printf "%s" (Token.show !tok);
    tok := (Lexer.read buffer)
  done;
  printf "%s" (Token.show !tok)

(* return printable AST generated by parser *)
let parse buffer = try Parser.input Lexer.read buffer with
                     Parser.Error ->
                       printf "%s: Syntax error.\n" (Lexer.print_position buffer); []

(* file input *)
let from_file opy_code =
  let buffer = ref (Lexing.from_string opy_code) in
  if !debug then (
    printf "************ LEXER OUTPUT ************\n";
    Lexer.setup_file_input !buffer;
    print_lex !buffer;
    buffer := Lexing.from_string opy_code (* reset buffer *)
  );
  Lexer.setup_file_input !buffer;
  let tree = parse !buffer in
  let instrs = Bytecode.compile_prog tree in
  if !debug then (
    printf "************ PARSER OUTPUT ************\n";
    printf "%s\n" (Ast.show tree);
    printf "************ BYTECODE ************\n";
    Bytecode.print_asm instrs;
    printf "************ CONSOLE OUTPUT ************\n"
  );
  envr := Interpreter.interpret instrs !envr

(* quit repl *)
let quit _ =
  printf "\nIch sterbe.\n";
  exit 0

(* read-eval-print loop *)
let rec repl () =
  try
    printf "]=> ";
    flush stdout;
    let buffer = Lexing.from_channel stdin in
    Lexer.setup_repl_input buffer;
    let tree = parse buffer in
    let instrs = Bytecode.compile_prog tree in
    if !debug then (
      printf "************ PARSER OUTPUT ************\n";
      printf "%s\n" (Ast.show tree);
      printf "************ BYTECODE ************\n";
      Bytecode.print_asm instrs;
      printf "************ CONSOLE OUTPUT ************\n"
    );
    (* interpret instructions and update global environment pointer *)
    envr := Interpreter.interpret instrs !envr;
    repl ()
  with e ->
    let msg = Printexc.to_string e in
    printf "Error: %s\n" msg;
    repl ()

(* currently prints lex output only *)
let rec lex_only () =
  printf "LEX-ONLY]=> ";
  flush stdout;
  let buffer = Lexing.from_channel stdin in
  Lexer.setup_repl_input buffer;
  printf "************ LEXER OUTPUT ************\n";
  print_lex buffer;
  lex_only ()

let handle_file filename =
  if Filename.extension filename <> ".opy" then
    raise (File_error "File extension not supported.")
  else
    match str_from_filename filename with
      Some s -> from_file s
    | None   -> printf "Failed to read from file.\n"

let handle_interactive () =
  (* start interactive mode *)
  printf "+----------------------------------------------+\n";
  printf "|             OPYTHN INTERACTIVE MODE          |\n";
  printf "|   Author: Marcel Goh (Release: 18.05.2019)   |\n";
  printf "|            Type \"Ctrl-C\" to quit.            |\n";
  printf "+----------------------------------------------+\n";
  flush stdout;
  if !lex then (
    lex_only ()
  )
  else (
    if !load <> "" then (
      handle_file !load
    );
    repl ()
  )

(* program entry point *)
let main () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle quit);
  Arg.parse (Arg.align speclist) set_filename usage_msg;
  if !lex || !load <> "" then (
    handle_interactive ()
  )
  else (
    match !filename with
      None ->
        handle_interactive ()
    | Some fname ->
        handle_file fname;
        exit 0
  )

let () = main ()
