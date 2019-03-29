(* File I/O *)

open Printf

exception File_error of string

(* opens the specified file and outputs its contents as a string *)
let str_from_filename filename =
  try
    let ch = open_in filename in
    let size = in_channel_length ch in
    let str = really_input_string ch size in
    close_in ch;
    Some str
  with e -> None

(* read a program argument as file *)
let str_of_prog_args () =
  if Array.length Sys.argv < 2 then
    raise (File_error "A file must be passed as input.")
  else
    let filename = Sys.argv.(1) in
    if Filename.extension filename <> ".opy" then
      raise (File_error "File extension not supported.")
    else
      str_from_filename filename
