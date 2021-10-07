Artjom Plaunov 
9/23/2021

This program is basically a "mini" C compiler I created to satisfy the assignment specs. The way this program works is it takes source code as input via STDIN, lexes the code to extract tokens, and feeds this into a parser-generator (menhir) genarated parser which turns the program into an AST. Then I simply traverse the AST with some custom recursive printing functions to print the data that the assignment asks for. 

No type checking is performed, so only well-formed programs can currently be processed. 

Note that a couple of the lexer/parser generator code snippets I had to find on the web to help me piece together the lexer and parser. But aside from that all the code is mine. 

The AST matches the grammar I designed for the given program; the type definitions in ast.ml are easy to read as the grammar of the program. Alternatively, the parser.mly file in the parse directory, which is used to generate the parser, also gives a good overview of the grammar and tokens. 
