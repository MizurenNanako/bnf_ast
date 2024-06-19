open Lexical
open Syntactics

let () =
  let filename = Sys.argv.(1) in
  let filech = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel filech in
  Lexing.set_filename lexbuf filename;
  let ast =
    try Parser.bnf_file Lexer.get_token lexbuf with
    | _ ->
      (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      |> Range.sexp_of_t
      |> Sexplib.Sexp.output_hum stdout;
      []
  in
  ast
  |> List.map AST.sexp_of_item
  |> List.iter (Sexplib.Sexp.output_hum stdout)
  |> print_newline
;;

(* let () =
  let filename = Sys.argv.(1) in
  let filech = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel filech in
  Lexing.set_filename lexbuf filename;
  let rec loop () =
    let tok = Lexer.get_token lexbuf in
    match tok with
    | Teof -> Printf.printf "\n"
    | k ->
      k
      |> Token.sexp_of_token
      |> Sexplib.Sexp.output_hum stdout
      |> print_newline;
      loop ()
  in
  loop ()
;; *)
