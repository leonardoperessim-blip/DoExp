parsetree3 <- function(node, parent_chain = character()) {
  if (node$type == "factor") return(list(list(kind="main", fmt=node$name, depth=length(parent_chain))))
  if (node$type == "nest") {
    A_terms <- parsetree3(node$parent, parent_chain)
    chain_names <- c(parent_chain, factorsfun(node$parent))
    B_base <- parsetree3(node$child, chain_names)
    B_nested <- lapply(B_base, \(x) list(kind="nested",
                                         fmt=sprintf("%s[%s]", x$fmt, paste(chain_names, collapse="^")),
                                         depth=length(chain_names)))
    return(c(A_terms, B_nested))
  }
  if (node$type == "cross") {
    L <- parsetree3(node$left,  parent_chain)
    R <- parsetree3(node$right, parent_chain)
    return(crossingfun(L, R))
  }
  if (node$type == "sum") {
    L <- parsetree3(node$left,  parent_chain)
    R <- parsetree3(node$right, parent_chain)
    return(c(L, R))
  }
}
