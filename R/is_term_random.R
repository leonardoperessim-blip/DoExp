is_term_random <- function(term, random_vec) {
  if (is.na(term) || !nzchar(term)) return(FALSE)

  term <- as.character(term)
  random_vec <- as.character(random_vec)

  facs <- strsplit(term, ":", fixed = TRUE)[[1]]

  (term %in% random_vec) || any(facs %in% random_vec)
}
