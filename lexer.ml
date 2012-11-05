# 2 "lexer.mll"
 

  open Lexing
  open Parser
  open Ast

  (* Erreurs lexicales *)

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ "void", VOID; "int", INT; "char", CHAR; "struct", STRUCT;
        "union", UNION; "sizeof", SIZEOF;
        "if", IF; "else", ELSE; "for", FOR; "while", WHILE;
        "return", RETURN ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum;
        pos_cnum=0 }

  let char_error s = raise (Lexical_error ("illegal character sequence: " ^ s))
  let decode_char s =
    match String.length s with
      1 -> Char.code s.[0]
    | 2 | 4 when s.[0] == '\\' -> begin
      match s.[1] with
      | 'n' -> 10 (* Char.code '\n' *)
      | 't' -> 9  (* Char.code '\t' *)
      | '\'' -> 39 (* Char.code '\'' *)
      | '\"' -> 34 (* Char.code '\"' *)
      | 'x' -> s.[0] <- '0'; (int_of_string s)
      | _ -> char_error s
    end
    | _ -> char_error s

  let str_buff = Buffer.create 512

# 45 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\218\255\219\255\002\000\003\000\031\000\002\000\020\000\
    \033\000\233\255\235\255\053\000\082\000\239\255\240\255\241\255\
    \242\255\243\255\244\255\245\255\246\255\247\255\248\255\145\000\
    \080\000\192\000\096\000\003\000\255\255\129\000\253\255\252\255\
    \216\000\101\000\249\255\033\001\056\001\220\255\238\255\221\255\
    \222\255\230\255\229\255\223\255\226\255\224\255\167\000\252\255\
    \253\255\254\255\094\000\255\255\168\001\252\255\253\255\254\255\
    \026\001\255\255\164\001\187\001";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\030\000\028\000\027\000\037\000\024\000\
    \023\000\255\255\255\255\019\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\037\000\
    \005\000\004\000\021\000\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
    \255\255\255\255\255\255\255\255\000\000\029\000\000\000\000\000\
    \255\255\255\255\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
    \000\000\000\000\255\255\000\000\057\000\000\000\000\000\000\000\
    \255\255\000\000\255\255\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\028\000\000\000\027\000\027\000\000\000\000\000\
    \027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\008\000\022\000\027\000\000\000\009\000\007\000\023\000\
    \019\000\018\000\010\000\011\000\015\000\012\000\013\000\026\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\041\000\014\000\003\000\005\000\004\000\045\000\
    \044\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\017\000\043\000\016\000\040\000\025\000\
    \039\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\021\000\006\000\020\000\042\000\037\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\030\000\031\000\034\000\051\000\000\000\029\000\
    \038\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\049\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\033\000\000\000\000\000\000\000\000\000\033\000\
    \002\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\000\000\000\000\000\000\000\000\025\000\
    \000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\000\000\057\000\000\000\000\000\000\000\
    \000\000\057\000\000\000\000\000\000\000\000\000\033\000\000\000\
    \000\000\000\000\000\000\000\000\033\000\000\000\000\000\000\000\
    \035\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
    \000\000\031\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \057\000\000\000\000\000\000\000\000\000\000\000\057\000\000\000\
    \000\000\255\255\058\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\048\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \000\000\000\000\055\000\000\000\000\000\000\000\000\000\053\000\
    \000\000\000\000\000\000\000\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\000\000\000\000\056\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \054\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\027\000\000\000\255\255\255\255\
    \027\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\027\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\007\000\000\000\000\000\000\000\000\000\003\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\000\000\008\000\000\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\006\000\012\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\026\000\029\000\033\000\050\000\255\255\026\000\
    \012\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\046\000\255\255\023\000\255\255\255\255\255\255\255\255\
    \023\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\046\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\023\000\255\255\255\255\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\032\000\255\255\255\255\255\255\255\255\032\000\
    \000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\255\255\255\255\255\255\255\255\025\000\
    \255\255\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\255\255\056\000\255\255\255\255\255\255\
    \255\255\056\000\255\255\255\255\255\255\255\255\032\000\255\255\
    \255\255\255\255\255\255\255\255\032\000\255\255\255\255\255\255\
    \032\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\035\000\035\000\035\000\035\000\035\000\035\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \255\255\029\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \056\000\255\255\255\255\255\255\255\255\255\255\056\000\255\255\
    \255\255\023\000\056\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\046\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\255\255\052\000\255\255\255\255\255\255\255\255\052\000\
    \255\255\255\255\255\255\255\255\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\058\000\058\000\058\000\
    \058\000\058\000\058\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\059\000\059\000\059\000\059\000\
    \059\000\059\000\255\255\255\255\052\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\059\000\059\000\059\000\059\000\
    \059\000\059\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \052\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 57 "lexer.mll"
      ( newline lexbuf; token lexbuf )
# 273 "lexer.ml"

  | 1 ->
# 59 "lexer.mll"
      ( token lexbuf )
# 278 "lexer.ml"

  | 2 ->
# 61 "lexer.mll"
      ( comment lexbuf; token lexbuf )
# 283 "lexer.ml"

  | 3 ->
# 63 "lexer.mll"
      ( newline lexbuf; token lexbuf )
# 288 "lexer.ml"

  | 4 ->
# 65 "lexer.mll"
      ( id_or_keyword (lexeme lexbuf) )
# 293 "lexer.ml"

  | 5 ->
let
# 66 "lexer.mll"
              s
# 299 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 67 "lexer.mll"
      (
	try
	  INTEGER (Int32.of_string s)
	with _ ->
	  raise (Lexical_error ("invalid integer constant '" ^ s ^ "'"))
      )
# 308 "lexer.ml"

  | 6 ->
let
# 73 "lexer.mll"
                  s
# 314 "lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 74 "lexer.mll"
      ( INTEGER (Int32.of_int (decode_char s)) )
# 318 "lexer.ml"

  | 7 ->
# 76 "lexer.mll"
      ( Buffer.reset str_buff;
        string lexbuf )
# 324 "lexer.ml"

  | 8 ->
# 79 "lexer.mll"
      ( LBRACE )
# 329 "lexer.ml"

  | 9 ->
# 81 "lexer.mll"
      ( RBRACE )
# 334 "lexer.ml"

  | 10 ->
# 83 "lexer.mll"
      ( LPAR )
# 339 "lexer.ml"

  | 11 ->
# 85 "lexer.mll"
      ( RPAR )
# 344 "lexer.ml"

  | 12 ->
# 87 "lexer.mll"
      ( LSQUARE )
# 349 "lexer.ml"

  | 13 ->
# 89 "lexer.mll"
      ( RSQUARE )
# 354 "lexer.ml"

  | 14 ->
# 91 "lexer.mll"
      ( COMMA )
# 359 "lexer.ml"

  | 15 ->
# 93 "lexer.mll"
      ( SEMICOLON )
# 364 "lexer.ml"

  | 16 ->
# 95 "lexer.mll"
      ( DOT )
# 369 "lexer.ml"

  | 17 ->
# 97 "lexer.mll"
      ( ARROW )
# 374 "lexer.ml"

  | 18 ->
# 99 "lexer.mll"
      ( MINUS )
# 379 "lexer.ml"

  | 19 ->
# 101 "lexer.mll"
      ( PLUS )
# 384 "lexer.ml"

  | 20 ->
# 103 "lexer.mll"
      ( STAR )
# 389 "lexer.ml"

  | 21 ->
# 105 "lexer.mll"
      ( SLASH )
# 394 "lexer.ml"

  | 22 ->
# 107 "lexer.mll"
      ( PERCENT )
# 399 "lexer.ml"

  | 23 ->
# 109 "lexer.mll"
      ( BANG )
# 404 "lexer.ml"

  | 24 ->
# 111 "lexer.mll"
      ( AMPERSAND )
# 409 "lexer.ml"

  | 25 ->
# 113 "lexer.mll"
      ( AND )
# 414 "lexer.ml"

  | 26 ->
# 115 "lexer.mll"
      ( OR )
# 419 "lexer.ml"

  | 27 ->
# 117 "lexer.mll"
      ( EQ )
# 424 "lexer.ml"

  | 28 ->
# 119 "lexer.mll"
      ( COMP Bgt )
# 429 "lexer.ml"

  | 29 ->
# 121 "lexer.mll"
      ( COMP Bge )
# 434 "lexer.ml"

  | 30 ->
# 123 "lexer.mll"
      ( COMP Blt )
# 439 "lexer.ml"

  | 31 ->
# 125 "lexer.mll"
      ( COMP Ble )
# 444 "lexer.ml"

  | 32 ->
# 127 "lexer.mll"
      ( EQOP Beq )
# 449 "lexer.ml"

  | 33 ->
# 129 "lexer.mll"
      ( EQOP Bneq )
# 454 "lexer.ml"

  | 34 ->
# 131 "lexer.mll"
      ( PLUSPLUS )
# 459 "lexer.ml"

  | 35 ->
# 133 "lexer.mll"
      ( MINUSMINUS )
# 464 "lexer.ml"

  | 36 ->
# 135 "lexer.mll"
      ( EOF )
# 469 "lexer.ml"

  | 37 ->
# 137 "lexer.mll"
      ( raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) )
# 474 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 46
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 140 "lexer.mll"
         ( () )
# 485 "lexer.ml"

  | 1 ->
# 141 "lexer.mll"
         ( newline lexbuf; comment lexbuf )
# 490 "lexer.ml"

  | 2 ->
# 142 "lexer.mll"
         ( raise (Lexical_error "unterminated comment") )
# 495 "lexer.ml"

  | 3 ->
# 143 "lexer.mll"
         ( comment lexbuf )
# 500 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 52
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 146 "lexer.mll"
            c
# 512 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 146 "lexer.mll"
              ( Buffer.add_char str_buff (Char.chr (decode_char c));
                string lexbuf )
# 517 "lexer.ml"

  | 1 ->
# 148 "lexer.mll"
         ( STRING (Buffer.contents str_buff) )
# 522 "lexer.ml"

  | 2 ->
# 149 "lexer.mll"
         ( raise (Lexical_error "unterminated string") )
# 527 "lexer.ml"

  | 3 ->
# 150 "lexer.mll"
      ( char_error (lexeme lexbuf) )
# 532 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

;;
