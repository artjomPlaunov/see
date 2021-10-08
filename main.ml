open Core
open Parse.FrontEnd
open Ast
open CodeGen

let lexbuf = Lexing.from_channel In_channel.stdin 
let res = (parse_program lexbuf)
let () = 
  match res with
    | Ok ast  ->  print_string (show_program ast);
                  genProg ast "output.s";
    | Error _ -> print_string "error"


