is_term_random <- function(term, random_vec) {
  if (is.na(term) || !nzchar(term)) return(FALSE)
  facs <- strsplit(term, ":", fixed = TRUE)[[1]]
  any(facs %in% random_vec)
}
