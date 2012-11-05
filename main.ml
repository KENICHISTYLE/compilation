(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast
open Typage

let usage = "usage: compilo [options] file.c"

let parse_only = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc
let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
   (* typage *)
    let typedp = typage p in 
    exit 0;
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "Syntax error\n@.";
	exit 1
    | Typage.Type_Error (l,msg) ->
        report_loc ((snd l), (fst l));
        eprintf "Typage error : %s\n@."  msg;
        exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
