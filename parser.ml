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

open Parsing;;
# 4 "parser.mly"
  open Ast

  (* déclarateurs:
     représentation intermédiaire permettant de convertir par ex:
     int x, **y, *z[10];
     en (int, x); (int**, y), (int*[10], z)
  *)

  let mk_loc e l = { loc = l; node = e }

  let loc e =
    mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

  let loc_i i e =
    mk_loc e (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

  let loc_dummy e =
    mk_loc e (Lexing.dummy_pos, Lexing.dummy_pos)


  type declarator =
    | Dident of ident
    | Dpointer of declarator
    | Darray of declarator * int

  let rec declarator ty = function
    | Dident id -> ty, id
    | Dpointer d -> declarator (Tpointer ty) d
    | Darray (d,s) -> declarator (Tarray (ty, s)) d

# 75 "parser.ml"
let yytransl_const = [|
  260 (* FOR *);
  261 (* IF *);
  262 (* ELSE *);
  263 (* WHILE *);
  264 (* RETURN *);
  265 (* SIZEOF *);
  266 (* VOID *);
  267 (* INT *);
  268 (* CHAR *);
  269 (* STRUCT *);
  270 (* UNION *);
  271 (* LPAR *);
  272 (* RPAR *);
  273 (* LBRACE *);
  274 (* RBRACE *);
  275 (* LSQUARE *);
  276 (* RSQUARE *);
  277 (* SEMICOLON *);
  278 (* COMMA *);
  279 (* DOT *);
  280 (* ARROW *);
    0 (* EOF *);
  281 (* EQ *);
  282 (* OR *);
  283 (* AND *);
  286 (* PLUS *);
  287 (* MINUS *);
  288 (* STAR *);
  289 (* SLASH *);
  290 (* PERCENT *);
  291 (* PLUSPLUS *);
  292 (* MINUSMINUS *);
  293 (* BANG *);
  294 (* AMPERSAND *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* INTEGER *);
  259 (* STRING *);
  284 (* EQOP *);
  285 (* COMP *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\007\000\
\004\000\006\000\006\000\008\000\008\000\013\000\013\000\010\000\
\010\000\010\000\010\000\010\000\012\000\012\000\011\000\011\000\
\011\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\015\000\015\000\
\018\000\018\000\016\000\016\000\019\000\019\000\020\000\020\000\
\009\000\005\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\001\000\006\000\006\000\005\000\002\000\
\003\000\000\000\002\000\000\000\001\000\002\000\004\000\001\000\
\001\000\001\000\002\000\002\000\001\000\003\000\001\000\002\000\
\004\000\003\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\002\000\002\000\002\000\002\000\001\000\001\000\004\000\001\000\
\002\000\004\000\003\000\003\000\003\000\001\000\002\000\005\000\
\007\000\009\000\005\000\001\000\002\000\003\000\001\000\002\000\
\000\000\001\000\000\000\001\000\001\000\003\000\000\000\002\000\
\004\000\001\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\075\000\000\000\016\000\017\000\018\000\000\000\
\000\000\001\000\003\000\004\000\000\000\000\000\074\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\019\000\020\000\000\000\
\000\000\000\000\000\000\022\000\011\000\000\000\000\000\000\000\
\007\000\000\000\025\000\005\000\006\000\000\000\015\000\045\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\061\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\030\000\072\000\073\000\000\000\000\000\068\000\
\000\000\000\000\062\000\000\000\000\000\053\000\000\000\000\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\064\000\040\000\047\000\050\000\070\000\000\000\000\000\000\000\
\059\000\000\000\000\000\000\000\057\000\000\000\058\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\034\000\072\000\035\000\013\000\027\000\
\073\000\036\000\043\000\022\000\029\000\074\000\117\000\111\000\
\075\000\143\000\112\000\076\000"

let yysindex = "\017\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\023\255\
\023\255\000\000\000\000\000\000\013\255\006\255\000\000\025\255\
\032\255\103\255\006\255\000\000\055\255\027\255\103\255\103\255\
\023\255\023\255\034\255\006\255\000\000\035\255\053\255\006\255\
\000\000\103\255\062\255\006\255\064\255\000\000\000\000\058\255\
\057\255\063\255\055\255\000\000\000\000\066\255\080\255\103\255\
\000\000\103\255\000\000\000\000\000\000\165\255\000\000\000\000\
\000\000\069\255\070\255\087\255\189\255\088\255\213\255\000\000\
\213\255\213\255\213\255\213\255\213\255\213\255\213\255\090\255\
\000\000\017\001\165\255\091\255\213\255\213\255\213\255\000\000\
\035\001\103\255\023\000\037\255\037\255\037\255\037\255\037\255\
\037\255\037\255\213\255\213\255\000\000\023\255\023\255\213\255\
\213\255\213\255\213\255\213\255\213\255\213\255\213\255\213\255\
\213\255\000\000\000\000\000\000\000\000\053\001\097\255\000\000\
\044\000\065\000\000\000\076\255\095\255\000\000\106\255\071\001\
\000\000\000\000\089\001\107\001\125\001\223\255\143\001\161\001\
\161\001\037\255\037\255\037\255\213\255\213\255\165\255\165\255\
\000\000\000\000\000\000\000\000\000\000\089\001\102\255\131\255\
\000\000\213\255\165\255\117\255\000\000\165\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\255\
\009\255\124\255\000\000\000\000\002\255\000\000\123\255\123\255\
\000\000\000\000\000\000\000\000\000\000\246\254\000\000\000\000\
\000\000\089\255\000\000\000\000\000\000\000\000\000\000\000\000\
\130\255\000\000\122\255\000\000\000\000\000\000\000\000\089\255\
\000\000\000\000\000\000\000\000\000\000\129\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\129\255\000\000\128\255\000\000\000\000\000\000\
\000\000\000\000\000\000\086\000\105\000\124\000\143\000\162\000\
\181\000\200\000\135\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\255\000\000\000\000\
\000\000\000\000\000\000\136\255\000\000\000\000\000\000\000\000\
\000\000\000\000\031\255\163\255\214\255\228\001\214\001\182\001\
\198\001\219\000\238\000\001\001\000\000\133\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\134\255\000\000\127\255\
\000\000\135\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\152\000\250\255\247\255\000\000\000\000\
\120\000\252\255\067\000\129\000\121\000\222\255\000\000\169\255\
\003\000\000\000\042\000\101\000"

let yytablesize = 768
let yytable = "\014\000\
\010\000\016\000\017\000\119\000\024\000\024\000\015\000\020\000\
\019\000\020\000\024\000\024\000\020\000\028\000\037\000\069\000\
\008\000\001\000\038\000\039\000\069\000\020\000\021\000\015\000\
\045\000\020\000\081\000\018\000\083\000\020\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\019\000\054\000\019\000\
\020\000\023\000\110\000\113\000\114\000\028\000\026\000\033\000\
\024\000\040\000\026\000\026\000\026\000\031\000\042\000\092\000\
\110\000\120\000\148\000\094\000\095\000\123\000\124\000\125\000\
\126\000\127\000\128\000\129\000\130\000\131\000\132\000\106\000\
\107\000\031\000\048\000\031\000\032\000\116\000\050\000\046\000\
\021\000\047\000\051\000\077\000\078\000\030\000\052\000\121\000\
\122\000\010\000\010\000\010\000\010\000\010\000\041\000\010\000\
\010\000\010\000\110\000\142\000\053\000\079\000\082\000\010\000\
\091\000\010\000\010\000\137\000\109\000\010\000\138\000\110\000\
\005\000\006\000\007\000\025\000\026\000\134\000\010\000\010\000\
\010\000\139\000\146\000\010\000\010\000\010\000\010\000\056\000\
\056\000\056\000\056\000\056\000\150\000\056\000\056\000\056\000\
\147\000\144\000\145\000\012\000\010\000\056\000\021\000\056\000\
\056\000\014\000\071\000\056\000\067\000\149\000\067\000\063\000\
\151\000\065\000\066\000\012\000\056\000\056\000\056\000\049\000\
\044\000\056\000\056\000\056\000\056\000\015\000\056\000\057\000\
\058\000\059\000\055\000\060\000\061\000\062\000\141\000\108\000\
\000\000\000\000\031\000\063\000\000\000\048\000\031\000\031\000\
\031\000\064\000\000\000\031\000\031\000\015\000\056\000\057\000\
\000\000\000\000\065\000\066\000\067\000\062\000\000\000\068\000\
\069\000\070\000\071\000\063\000\000\000\000\000\000\000\000\000\
\000\000\080\000\000\000\000\000\000\000\015\000\056\000\057\000\
\000\000\000\000\065\000\066\000\067\000\062\000\000\000\068\000\
\069\000\070\000\071\000\063\000\000\000\032\000\000\000\000\000\
\000\000\032\000\032\000\032\000\000\000\000\000\032\000\032\000\
\032\000\092\000\065\000\066\000\067\000\094\000\095\000\068\000\
\069\000\070\000\071\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\000\000\
\000\000\048\000\000\000\000\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\118\000\000\000\
\000\000\092\000\000\000\000\000\000\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\135\000\000\000\000\000\092\000\000\000\
\000\000\000\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\136\000\000\000\000\000\092\000\000\000\000\000\000\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\041\000\000\000\000\000\
\000\000\041\000\041\000\041\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\042\000\000\000\000\000\000\000\042\000\042\000\042\000\000\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\049\000\000\000\000\000\000\000\049\000\
\049\000\049\000\000\000\000\000\049\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\028\000\000\000\
\000\000\000\000\028\000\028\000\028\000\000\000\000\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\027\000\000\000\000\000\000\000\027\000\027\000\027\000\
\000\000\000\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\043\000\000\000\000\000\000\000\
\043\000\043\000\043\000\000\000\000\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\044\000\
\000\000\000\000\000\000\044\000\044\000\044\000\000\000\000\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\037\000\000\000\000\000\000\000\037\000\037\000\
\037\000\000\000\000\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\038\000\000\000\000\000\
\000\000\038\000\038\000\038\000\000\000\000\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\039\000\000\000\000\000\000\000\039\000\039\000\039\000\000\000\
\000\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\092\000\000\000\093\000\000\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\092\000\000\000\115\000\
\000\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\092\000\
\000\000\000\000\133\000\094\000\095\000\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\092\000\140\000\000\000\000\000\094\000\095\000\096\000\
\097\000\098\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\092\000\000\000\000\000\000\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\092\000\000\000\000\000\
\000\000\094\000\095\000\000\000\000\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\092\000\
\000\000\000\000\000\000\094\000\095\000\000\000\000\000\000\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\092\000\000\000\000\000\000\000\094\000\095\000\000\000\
\000\000\000\000\000\000\000\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\092\000\000\000\000\000\000\000\094\000\
\095\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\103\000\104\000\105\000\106\000\107\000\035\000\000\000\000\000\
\000\000\035\000\035\000\035\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\036\000\000\000\000\000\
\000\000\036\000\036\000\036\000\000\000\000\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\034\000\000\000\000\000\
\000\000\034\000\034\000\034\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\033\000\000\000\000\000\000\000\033\000\
\033\000\033\000\000\000\000\000\033\000\033\000\033\000\033\000"

let yycheck = "\004\000\
\000\000\008\000\009\000\091\000\015\001\016\001\001\001\014\000\
\001\001\001\001\021\001\022\001\019\000\018\000\024\000\016\001\
\015\001\001\000\025\000\026\000\021\001\028\000\021\001\001\001\
\034\000\032\000\061\000\015\001\063\000\036\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\032\001\048\000\032\001\
\032\001\017\001\077\000\078\000\079\000\050\000\016\001\021\001\
\017\001\016\001\020\001\021\001\022\001\019\001\002\001\019\001\
\091\000\092\000\146\000\023\001\024\001\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\035\001\
\036\001\019\001\017\001\019\001\022\001\082\000\022\001\018\001\
\014\000\018\001\020\001\015\001\015\001\019\000\021\001\094\000\
\095\000\001\001\002\001\003\001\004\001\005\001\028\000\007\001\
\008\001\009\001\133\000\134\000\021\001\015\001\015\001\015\001\
\015\001\017\001\018\001\032\001\018\001\021\001\016\001\146\000\
\010\001\011\001\012\001\013\001\014\001\021\001\030\001\031\001\
\032\001\016\001\021\001\035\001\036\001\037\001\038\001\001\001\
\002\001\003\001\004\001\005\001\016\001\007\001\008\001\009\001\
\006\001\135\000\136\000\016\001\018\001\015\001\021\001\017\001\
\018\001\016\001\018\001\021\001\021\001\147\000\016\001\016\001\
\150\000\021\001\021\001\004\000\030\001\031\001\032\001\040\000\
\032\000\035\001\036\001\037\001\038\001\001\001\002\001\003\001\
\004\001\005\001\050\000\007\001\008\001\009\001\133\000\075\000\
\255\255\255\255\016\001\015\001\255\255\017\001\020\001\021\001\
\022\001\021\001\255\255\025\001\026\001\001\001\002\001\003\001\
\255\255\255\255\030\001\031\001\032\001\009\001\255\255\035\001\
\036\001\037\001\038\001\015\001\255\255\255\255\255\255\255\255\
\255\255\021\001\255\255\255\255\255\255\001\001\002\001\003\001\
\255\255\255\255\030\001\031\001\032\001\009\001\255\255\035\001\
\036\001\037\001\038\001\015\001\255\255\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\019\001\030\001\031\001\032\001\023\001\024\001\035\001\
\036\001\037\001\038\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\010\001\011\001\012\001\013\001\014\001\255\255\
\255\255\016\001\255\255\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\016\001\255\255\
\255\255\019\001\255\255\255\255\255\255\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\016\001\255\255\255\255\019\001\255\255\
\255\255\255\255\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\016\001\255\255\255\255\019\001\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\016\001\255\255\255\255\255\255\020\001\021\001\022\001\255\255\
\255\255\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\016\001\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\255\255\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\016\001\255\255\
\255\255\255\255\020\001\021\001\022\001\255\255\255\255\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\016\001\255\255\255\255\255\255\020\001\021\001\022\001\
\255\255\255\255\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\016\001\255\255\255\255\255\255\
\020\001\021\001\022\001\255\255\255\255\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\016\001\
\255\255\255\255\255\255\020\001\021\001\022\001\255\255\255\255\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\016\001\255\255\255\255\255\255\020\001\021\001\
\022\001\255\255\255\255\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\016\001\255\255\255\255\255\255\020\001\021\001\022\001\255\255\
\255\255\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\019\001\255\255\021\001\
\255\255\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\019\001\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\019\001\020\001\255\255\255\255\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\019\001\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\019\001\255\255\255\255\
\255\255\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\019\001\
\255\255\255\255\255\255\023\001\024\001\255\255\255\255\255\255\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\019\001\255\255\255\255\255\255\023\001\024\001\255\255\
\255\255\255\255\255\255\255\255\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\019\001\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001\033\001\034\001\035\001\036\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\016\001\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\255\255\025\001\026\001\027\001\028\001"

let yynames_const = "\
  FOR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  RETURN\000\
  SIZEOF\000\
  VOID\000\
  INT\000\
  CHAR\000\
  STRUCT\000\
  UNION\000\
  LPAR\000\
  RPAR\000\
  LBRACE\000\
  RBRACE\000\
  LSQUARE\000\
  RSQUARE\000\
  SEMICOLON\000\
  COMMA\000\
  DOT\000\
  ARROW\000\
  EOF\000\
  EQ\000\
  OR\000\
  AND\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  PERCENT\000\
  PLUSPLUS\000\
  MINUSMINUS\000\
  BANG\000\
  AMPERSAND\000\
  "

let yynames_block = "\
  IDENT\000\
  INTEGER\000\
  STRING\000\
  EQOP\000\
  COMP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl_star) in
    Obj.repr(
# 87 "parser.mly"
    ( List.rev _1 )
# 467 "parser.ml"
               : Ast.loc Ast.file))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                ( [] )
# 473 "parser.ml"
               : 'decl_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl_star) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 94 "parser.mly"
                 ( _2 :: _1 )
# 481 "parser.ml"
               : 'decl_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl_vars) in
    Obj.repr(
# 99 "parser.mly"
    ( Dvars _1 )
# 488 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'decl_vars_star) in
    Obj.repr(
# 101 "parser.mly"
    ( Dstruct (_2, _4) )
# 496 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'decl_vars_star) in
    Obj.repr(
# 103 "parser.mly"
    ( Dunion (_2, _4) )
# 504 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'c_type_fun) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'parameters_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 105 "parser.mly"
    ( let ty, id = _1 in Dfun (ty, id, _3, _5) )
# 513 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'c_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 110 "parser.mly"
    ( match declarator _1 _2 with
    | Tarray _, _ -> raise Parsing.Parse_error
    | tid -> tid )
# 523 "parser.ml"
               : 'c_type_fun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'c_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vars) in
    Obj.repr(
# 116 "parser.mly"
                        ( List.map (declarator _1) _2 )
# 531 "parser.ml"
               : 'decl_vars))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                ( [] )
# 537 "parser.ml"
               : 'decl_vars_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl_vars) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_vars_star) in
    Obj.repr(
# 121 "parser.mly"
                           ( _1 @ _2 )
# 545 "parser.ml"
               : 'decl_vars_star))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                ( [] )
# 551 "parser.ml"
               : 'parameters_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parameters) in
    Obj.repr(
# 126 "parser.mly"
                ( _1 )
# 558 "parser.ml"
               : 'parameters_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'c_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 130 "parser.mly"
             ( [declarator _1 _2] )
# 566 "parser.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'c_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'parameters) in
    Obj.repr(
# 131 "parser.mly"
                              ( declarator _1 _2 :: _4 )
# 575 "parser.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
       ( Tvoid )
# 581 "parser.ml"
               : 'c_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
      ( Tint )
# 587 "parser.ml"
               : 'c_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
       ( Tchar )
# 593 "parser.ml"
               : 'c_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 138 "parser.mly"
               ( Tstruct _2 )
# 600 "parser.ml"
               : 'c_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 139 "parser.mly"
               ( Tunion _2 )
# 607 "parser.ml"
               : 'c_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 143 "parser.mly"
      ( [_1] )
# 614 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 144 "parser.mly"
                 ( _1 :: _3 )
# 622 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 149 "parser.mly"
    ( Dident _1 )
# 629 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 151 "parser.mly"
    ( Dpointer _2 )
# 636 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int32) in
    Obj.repr(
# 153 "parser.mly"
    ( Darray (_1, Int32.to_int _3) )
# 644 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
    ( loc (Eassign (_1, _3)) )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
    ( loc (Eunop (Upre_dec, _2)) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
    ( loc (Eunop (Upre_inc, _2)) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
    ( loc (Eunop (Upost_inc, _1)) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
    ( loc (Eunop (Upost_dec, _1)) )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
    ( loc (Ebinop (Bor, _1, _3)) )
# 688 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
    ( loc (Ebinop (Band, _1, _3)) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
    ( loc (Ebinop (_2, _1, _3)) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
    ( loc (Ebinop (_2, _1, _3)) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
    ( loc (Ebinop (Badd, _1, _3)) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
    ( loc (Ebinop (Bsub, _1, _3)) )
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "parser.mly"
    ( loc (Ebinop (Bmul, _1, _3)) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
    ( loc (Ebinop (Bdiv, _1, _3)) )
# 746 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "parser.mly"
    ( loc (Ebinop (Bmod, _1, _3)) )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'cplx_type) in
    Obj.repr(
# 186 "parser.mly"
    ( loc (Esizeof (_3)) )
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
    ( loc (Eunop (Uplus, _2)) )
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
    ( loc (Eunop (Uminus, _2)) )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
    ( loc (Eunop (Unot, _2)) )
# 782 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
    ( loc (Eunop (Uamp, _2)) )
# 789 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 196 "parser.mly"
    ( if _1 = 0l then loc Enull else loc (Econst (Cint _1)) )
# 796 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 198 "parser.mly"
    ( loc (Econst (Cstring _1)) )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'l_expr_opt) in
    Obj.repr(
# 200 "parser.mly"
    ( loc (Ecall (_1, _3)) )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 202 "parser.mly"
    ( loc (Eident _1) )
# 818 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 204 "parser.mly"
    ( loc (Eunop (Ustar, _2)) )
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 206 "parser.mly"
    ( 
      let sum = loc (Ebinop(Badd, _1, _3)) in
	loc (Eunop(Ustar,sum))
    )
# 836 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 211 "parser.mly"
    ( loc (Edot (_1, _3)) )
# 844 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 214 "parser.mly"
    (
      let star = loc (Eunop(Ustar, _1)) in
      loc (Edot (star, _3))
    )
# 855 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 219 "parser.mly"
    ( _2 )
# 862 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 224 "parser.mly"
   ( loc Sskip )
# 868 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 226 "parser.mly"
   ( loc (Sexpr _1) )
# 875 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 228 "parser.mly"
   ( loc (Sif (_3, _5, loc_dummy Sskip)) )
# 883 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 230 "parser.mly"
   ( loc (Sif (_3, _5, _7)) )
# 892 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'l_expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_or_1) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'l_expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 232 "parser.mly"
   ( let l_expr i = List.map (fun e -> loc_i i (Sexpr e)) in
     loc (Sfor (l_expr 3 _3, _5, l_expr 7 _7, _9)) )
# 903 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 235 "parser.mly"
   ( loc (Swhile (_3, _5)) )
# 911 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 237 "parser.mly"
   ( loc (Sblock _1) )
# 918 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 239 "parser.mly"
   ( loc (Sreturn None) )
# 924 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 241 "parser.mly"
   ( loc (Sreturn (Some _2)) )
# 931 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'c_type) in
    Obj.repr(
# 245 "parser.mly"
          ( _1 )
# 938 "parser.ml"
               : 'cplx_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'c_type) in
    Obj.repr(
# 246 "parser.mly"
              ( Tpointer _1 )
# 945 "parser.ml"
               : 'cplx_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 249 "parser.mly"
                (  loc (Econst (Cint 1l)) )
# 951 "parser.ml"
               : 'expr_or_1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 250 "parser.mly"
       ( _1 )
# 958 "parser.ml"
               : 'expr_or_1))
; (fun __caml_parser_env ->
    Obj.repr(
# 254 "parser.mly"
                ( [] )
# 964 "parser.ml"
               : 'l_expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'l_expr) in
    Obj.repr(
# 255 "parser.mly"
                ( _1 )
# 971 "parser.ml"
               : 'l_expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 259 "parser.mly"
       ( [_1] )
# 978 "parser.ml"
               : 'l_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'l_expr) in
    Obj.repr(
# 260 "parser.mly"
                    ( _1 :: _3 )
# 986 "parser.ml"
               : 'l_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 264 "parser.mly"
                ( [] )
# 992 "parser.ml"
               : 'statement_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement_star) in
    Obj.repr(
# 265 "parser.mly"
                           ( _1 :: _2 )
# 1000 "parser.ml"
               : 'statement_star))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decl_vars_star) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'statement_star) in
    Obj.repr(
# 269 "parser.mly"
                                              ( _2, _3 )
# 1008 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 275 "parser.mly"
        ( loc _1 )
# 1015 "parser.ml"
               : 'ident))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.loc Ast.file)
