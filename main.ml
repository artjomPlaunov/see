open Core
open Parse.FrontEnd
open Ast

let lexbuf = Lexing.from_channel In_channel.stdin 
let res = (parse_program lexbuf)
let () = 
  match res with
    | Ok ast  -> print_program ast
    | Error _ -> print_string "error"


