{
    open Lexical.Token
    open Lexical.Error
    open Lexical
}

let punctuation = ['+' '*' '?' '|' ',' ':' '(' ')' '\n']
let whitespace = [' ' '\t']

let identifier = (_ # punctuation # whitespace)+

rule get_token = parse
| whitespace { get_token lexbuf }
| '\n' { Lexing.new_line lexbuf; get_token lexbuf }
| '+' { Tplus (lexbuf.lex_start_p) }
| '*' { Tstar (lexbuf.lex_start_p) }
| '?' { Tquest (lexbuf.lex_start_p) }
| '|' { Tgun (lexbuf.lex_start_p) }
| '(' { Tlp (lexbuf.lex_start_p) }
| ')' { Trp (lexbuf.lex_start_p) }
| ',' { Tcomma (lexbuf.lex_start_p) }
| ';' { Tsemi (lexbuf.lex_start_p) }
| "::=" { Tdef (lexbuf.lex_start_p) }
| "\"" {
    let posL = lexbuf.lex_start_p in
    let ctx = get_string (Buffer.create 17) lexbuf in
    let posR = lexbuf.lex_curr_p in
    Tstring (ctx, (posL, posR))
}
| identifier as a { Tidentifier (a, Range.of_lexbuf lexbuf) }
| eof { Teof }

and get_string buf = parse
| '\"' { Buffer.contents buf }
| '\n' { Lexing.new_line lexbuf; get_string buf lexbuf }
| "\\\"" { Buffer.add_char buf '\"'; get_string buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; get_string buf lexbuf }
| "\\t" { Buffer.add_char buf '\t'; get_string buf lexbuf }
| _ as c { Buffer.add_char buf c; get_string buf lexbuf }
| eof { raise (LexicalError ("Unexpected eof in string")) }
