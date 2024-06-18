%{
    open Lexical.Token
    open Lexical
    open Syntactics.AST
%}

%token <string * Range.t> Tstring
%token <string * Range.t> Tidentifier

%token <Range.pos> Tlp "("
%token <Range.pos> Trp ")"
%token <Range.pos> Tgun "|"
%token <Range.pos> Tplus "+"
%token <Range.pos> Tstar "*"
%token <Range.pos> Tquest "?"
%token <Range.pos> Tcomma ","

%token <Range.pos> Tdef "::="
%token Teof

%start <t> bnf_file

%%

bnf_file:
| l = bnf_item_or_macro*; Teof; { l }

bnf_item_or_macro:
| i = bnf_item { i }
| m = macro { m }

macro:
| id = Tidentifier; "("; l = Tidentifier*; ")"; r = item_rules;
{
    {
        item_desc = ItemMacroDef {
            macdef_id = id;
            macdef_param = l;
            macdef_body = fst r;
        };
        item_rng = Range.join (snd id) (snd r);
    }
}

bnf_item:
| id = Tidentifier; "::="; r = item_rules; 
{
    {
        item_desc = ItemBNF {
            item_id = id;
            item_rules = fst r;
        };
        item_rng = Range.join (snd id) (snd r);
    }
}

item_rules:
| posL = "|"?; l = item_rules_; 
{
    match posL with
    | Some posL -> List.rev (fst l), Range.join (get_rng (Tgun posL)) (snd l)
    | None -> List.rev (fst l), snd l
}

item_rules_:
| l = item_rules_; "|"; i = rhs_item; 
{
    i :: (fst l), Range.join (snd l) i.rhs_item_rng
}
| i = rhs_item; { [i], i.rhs_item_rng }

rhs_item:
| i = Tidentifier
{
    {
        rhs_item_desc = Tid (i);
        rhs_item_rng = snd i;
    }
}
| i = Tstring
{
    {
        rhs_item_desc = Tstr (fst i);
        rhs_item_rng = snd i;
    }
}
| i = Tidentifier; "("; l = item_rules; posR = ")";
{
    {
        rhs_item_desc = Macro {
            macro_id = i;
            macro_args = fst l;
        };
        rhs_item_rng = Range.join (snd i) (get_rng (Trp posR))
    }
}
