break_M_label <- function(base, extras = character(0), n = 3) {
  terms <- c(base, extras)
  terms <- terms[!is.na(terms) & nzchar(terms)]

  if (length(terms) == 0) return("")

  groups <- split(terms, ceiling(seq_along(terms) / n))

  lines <- vapply(groups, function(g) {
    line <- paste(g, collapse = " ")
    sub("^\\+\\s*", "", line)
  }, character(1))

  paste(lines, collapse = "\n")
}
