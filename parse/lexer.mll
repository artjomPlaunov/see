{
  open Lexing
  open Parser

  exception LexerError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { 
      pos with  
        pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let digits = ['0'-'9']+
let id  = (alpha) (alpha|digit|'_')*

let whitespace = [' ' '\t']+
let newline = '\n'


rule read_token = 
  parse
  | "{"         { LBRACE    }
  | "}"         { RBRACE    }
  | "="         { EQ        }
  | "+"         { PLUS      }
  | ","         { COMMA     }
  | "("         { LPAREN    }
  | ")"         { RPAREN    }
  | ";"         { SEMICOLON }
  | "["         { LBRACK    }
  | "]"         { RBRACK    }
  | "int"       { INT_TYPE  }
  | "void"      { VOID_TYPE  }
  | "return"    { RETURN    }
  | whitespace  { read_token lexbuf }
  | digits      { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id          { ID  (Lexing.lexeme lexbuf) }
  | newline     { next_line lexbuf; read_token lexbuf }
  | eof         { EOF }
  | _           { raise (LexerError (
                          "Lexing: Unknown Character: " ^ Lexing.lexeme lexbuf
                        )) }
