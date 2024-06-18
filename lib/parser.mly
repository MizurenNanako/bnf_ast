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
%type <rule> rule
%type <rule list * rng> rules
%%

bnf_file:
| l = bnf_item_or_macro*; Teof; { l }

bnf_item_or_macro:
| id = Tidentifier; "("; l = Tidentifier*; ")"; r = rules;
{
    {
        item_desc = ItemMacroDef {
            macdef_id = fst id;
            macdef_param = l;
            macdef_body = fst r;
        };
        item_rng = Range.join (snd id) (snd r);
    }
}
| id = Tidentifier; "::="; r = rules; 
{
    {
        item_desc = ItemBNF {
            item_id = fst id;
            item_rules = fst r;
        };
        item_rng = Range.join (snd id) (snd r);
    }
}

rules:
| posL = "|"?; l = rules_; 
{
    match posL with
    | Some posL -> List.rev (fst l), Range.join (get_rng (Tgun posL)) (snd l)
    | None -> List.rev (fst l), snd l
}

rules_:
| l = rules_; "|"; i = rule; 
{
    i :: (fst l), Range.join (snd l) i.rule_rng
}
| i = rule; { [i], i.rule_rng }

rule:
| r = rule_; { { r with rule_desc = List.rev r.rule_desc} }

rule_:
| r = rhs_item; 
{
    {
        rule_desc = [r];
        rule_rng = r.rhs_item_rng;
    }
}
| l = rule_; r = rhs_item;
{
    {
        rule_desc = r :: l.rule_desc;
        rule_rng = Range.join l.rule_rng r.rhs_item_rng;
    }
}

rhs_item:
| i = Tidentifier
{
    {
        rhs_item_desc = Tid (fst i);
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
| i = Tidentifier; "("; l = rhs_item*; posR = ")";
{
    {
        rhs_item_desc = Macro {
            macro_id = fst i;
            macro_args = l;
        };
        rhs_item_rng = Range.join (snd i) (get_rng (Trp posR))
    }
}
