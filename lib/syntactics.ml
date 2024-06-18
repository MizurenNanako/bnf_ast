open Lexical
open Sexplib0.Sexp_conv

module AST = struct
  type item =
    { item_desc : item_desc
    ; item_rng : Range.t
    }
  [@@deriving sexp_of]

  and item_desc =
    | ItemBNF of
        { item_id : id
        ; item_rules : rule list
        }
    | ItemMacroDef of
        { macdef_id : id
        ; macdef_param : id ranged list
        ; macdef_body : rule list
        }

  and rule =
    { rule_desc : rhs_item list
    ; rule_rng : Range.t
    }

  and rhs_item =
    { rhs_item_desc : rhs_item_desc
    ; rhs_item_rng : Range.t
    }

  and rhs_item_desc =
    | Tid of id
    | Tstr of str
    | Macro of
        { macro_id : id
        ; macro_args : rhs_item list
        }

  and id = string
  and str = string

  type t = item list
end
