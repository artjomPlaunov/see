open Core
open Lexer

let parse_program lexbuf =
  try Ok (Parser.program Lexer.read_token lexbuf) with
  (* catch exception and turn into Error *)
  | LexerError _ ->
      let error_msg = "Lexer error" in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg = "Parser error" in
      Error (Error.of_string error_msg)
