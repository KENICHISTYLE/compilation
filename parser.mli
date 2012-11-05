type token =
  | IDENT of (string)
  | INTEGER of (int32)
  | STRING of (string)
  | FOR
  | IF
  | ELSE
  | WHILE
  | RETURN
  | SIZEOF
  | VOID
  | INT
  | CHAR
  | STRUCT
  | UNION
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | SEMICOLON
  | COMMA
  | DOT
  | ARROW
  | EOF
  | EQ
  | OR
  | AND
  | EQOP of (Ast.binop)
  | COMP of (Ast.binop)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | PLUSPLUS
  | MINUSMINUS
  | BANG
  | AMPERSAND

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.loc Ast.file
