(lang dune 3.15)

(using menhir 3.0)

(name bnf_ast)

(source
 (github MizurenNanako/bnf_ast))

(authors "Cherry Sigma")

(license public domain)

(package
 (name bnf_ast)
 (synopsis "bast")
 (description "A bnf parser and transpiler")
 (depends ocaml dune menhirLib ppx_sexp_conv))
