splitfun <- function(form) {
  form <- gsub("\\s+", "", form)
  m <- gregexpr("([A-Za-z][A-Za-z0-9_]*)|[()/*+]", form, perl = TRUE)[[1]]
  if (m[1] == -1) stop("Empty structural formulae")
  tokens <- regmatches(form, list(m))[[1]]
  types  <- ifelse(grepl("^[A-Za-z]", tokens), "ID", tokens)
  data.frame(tokens = tokens, type = types, stringsAsFactors = FALSE)
}
