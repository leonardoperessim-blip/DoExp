nestedselectionfun <- function(terms) {
  if (!any(vapply(terms, `[[`, "", "kind") == "nested")) {
    return(terms)
  }
  Filter(function(t) t$kind == "nested", terms)
}
