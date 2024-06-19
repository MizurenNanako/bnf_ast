module Range = struct
  type pos = Lexing.position

  let sexp_of_pos (pos : pos) =
    Sexplib0.Sexp.Atom
      (Printf.sprintf
         "%s:%i:%i"
         pos.pos_fname
         pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol + 1))
  ;;

  type t = pos * pos [@@deriving sexp_of]

  let of_lexbuf (lexbuf : Lexing.lexbuf) = lexbuf.lex_start_p, lexbuf.lex_curr_p
  let join a b : t = fst a, snd b
end

type 'a ranged = 'a * Range.t [@@deriving sexp_of]

module Error = struct
  exception LexicalError of string
end

module Token = struct
  open Sexplib0.Sexp_conv

  type rng = Range.t [@@deriving sexp_of]
  type pos = Range.pos [@@deriving sexp_of]

  type token =
    | Tstring of string ranged
    | Tidentifier of string ranged
    (* length of 1 *)
    | Tlp of pos (* ( *)
    | Trp of pos (* ) *)
    | Tgun of pos (* | *)
    | Tplus of pos (* + *)
    | Tstar of pos (* * *)
    | Tquest of pos (* ? *)
    | Tcomma of pos (* , *)
    | Tsemi of pos (* ; *)
    (* length of 3 *)
    | Tdef of pos (* ::= *)
    | Teof
  [@@deriving sexp_of]

  let get_rng = function
    | Tstring (_, r) | Tidentifier (_, r) -> r
    | Tlp p | Trp p | Tgun p | Tstar p | Tplus p | Tquest p | Tcomma p | Tsemi p
      -> p, { p with pos_cnum = p.pos_cnum + 1 }
    | Tdef p -> p, { p with pos_cnum = p.pos_cnum + 3 }
    | Teof -> Lexing.dummy_pos, Lexing.dummy_pos
  ;;
end
