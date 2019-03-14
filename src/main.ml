(* OPythn main front-end *)

open Printf

(* opens the specified file and outputs its contents as a string *)
let str_from_filename filename =
  try
    let ch = open_in filename in
    let size = in_channel_length ch in
    let str = really_input_string ch size in
    close_in ch;
    Some str
  with e -> None

let main () =
  if Array.length Sys.argv < 2 then
    printf "A file must be passed as input.\n"
  else
    let filename = Sys.argv.(1) in
    let len = String.length filename in
    if Filename.extension filename <> ".opy" then
      printf "File extension not supported.\n"
    else
      let contents = str_from_filename filename in
      match contents with
      Some s -> printf "%s" s
      | None -> printf "Failed to get text from file."

let _ = main ()
