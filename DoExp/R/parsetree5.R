parsetree5 <- function(formula_str) {
  ast   <- parsetree2(formula_str)
  terms <- parsetree3(ast, parent_chain = character())
  if (!length(terms)) return(data.frame(term=character(), kind=character(), depth=integer()))
  df <- unique(do.call(rbind, lapply(terms, \(t) data.frame(term=t$fmt, kind=t$kind, depth=t$depth, stringsAsFactors=FALSE))))
  rownames(df) <- NULL
  kind_order <- c("main","nested","inter")
  df[order(match(df$kind, kind_order), df$depth, df$term), , drop=FALSE]
}
