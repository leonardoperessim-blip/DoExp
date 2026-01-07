parsetree4 <- function(formula_str) {
  syntaxtree <- parsetree2(formula_str)
  terms <- parsetree3(syntaxtree,
                      parent_chain = character())
  uniq <- unique(vapply(terms, `[[`, "", "fmt"))
  paste(uniq, collapse = " + ")
}
